{-# OPTIONS_GHC -Wno-partial-fields #-}

module Abaks.Entities
  ( AbaksEvent (..),
    PeriodId (..),
    Entry (..),
    EntryId (..),
    Amount (..),
    EntryState (..),
    startPeriod,
    addEntry,
    changeAmountEntry,
    validateEntry,
    commentEntry,
    markInClonflictEntry,
    deleteEntry,
  )
where

import Control.Monad (unless)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Exts (IsString)
import GHC.Generics (Generic)

-- * Base types

newtype PeriodId = PeriodId {getPeriodId :: Int}
  deriving stock (Eq, Ord, Show, Generic)

data Entry = Entry
  { entryId :: EntryId,
    amount :: Amount,
    category :: Text,
    comment :: Text,
    state :: EntryState,
    date :: Day
  }
  deriving stock (Eq, Show, Generic)

newtype EntryId = EntryId {getEntryId :: Int}
  deriving stock (Eq, Ord, Show, Generic)

newtype Amount = Amount {getAmountInCents :: Int}
  deriving stock (Eq, Ord, Show, Generic)

data EntryState
  = Expected
  | Unexpected
  | Validated
  | InConflict Text
  deriving stock (Eq, Show, Generic)

-- * Events

data AbaksEvent
  = Started {periodId :: PeriodId, name :: Text, from :: Day, to :: Day, initialBalance :: Amount}
  | EntryAdded {entry :: Entry}
  | EntryAmountChanged {entryId :: EntryId, amount :: Amount}
  | EntryValidated {entryId :: EntryId}
  | EntryCommented {entryId :: EntryId, comment :: Text}
  | EntryMarkedInConflict {entryId :: EntryId, reason :: Text}
  | EntryDeleted {entryId :: EntryId, comment :: Text}
  deriving stock (Eq, Show, Generic)

-- * CommandHandler

startPeriod :: PeriodId -> Text -> Day -> Day -> Amount -> CommandHandler AbaksEvent ExplainedError
startPeriod periodId name from to balance events = do
  unless (null events) $
    Left "Period already started"
  return
    [ Started
        { periodId = periodId,
          name = name,
          from = from,
          to = to,
          initialBalance = balance
        }
    ]

addEntry :: Entry -> CommandHandler AbaksEvent ExplainedError
addEntry entry events = do
  hasStarted events
  inPeriod entry.date events
  let entries = listEntries events
  unless (Map.notMember entry.entryId entries) $
    Left "Entry already existing"
  return [EntryAdded entry]

changeAmountEntry :: EntryId -> Amount -> CommandHandler AbaksEvent ExplainedError
changeAmountEntry entryId amount events = do
  validEntry entryId events
  return [EntryAmountChanged entryId amount]

validateEntry :: EntryId -> CommandHandler AbaksEvent ExplainedError
validateEntry entryId events = do
  validEntry entryId events
  return [EntryValidated entryId]

commentEntry :: EntryId -> Text -> CommandHandler AbaksEvent ExplainedError
commentEntry entryId comment events = do
  validEntry entryId events
  return [EntryCommented entryId comment]

markInClonflictEntry :: EntryId -> Text -> CommandHandler AbaksEvent ExplainedError
markInClonflictEntry entryId reason events = do
  validEntry entryId events
  return [EntryMarkedInConflict entryId reason]

deleteEntry :: EntryId -> Text -> CommandHandler AbaksEvent ExplainedError
deleteEntry entryId comment events = do
  validEntry entryId events
  return [EntryDeleted entryId comment]

-- * Various command handler utils

newtype ExplainedError = ExplainedError {getExplainedError :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)

hasStarted :: Events AbaksEvent -> Either ExplainedError ()
hasStarted =
  \case
    (Started {} : _) -> return ()
    _ -> Left "Period is not properly defined"

inPeriod :: Day -> Events AbaksEvent -> Either ExplainedError ()
inPeriod x =
  \case
    (Started {..} : _) ->
      if x >= from && x <= to
        then return ()
        else Left "Out of period"
    _ -> Left "Period is not properly defined"

-- fetchEntry :: EntryId -> Events AbaksEvent -> Entry
-- fetchEntry x =
--   \case
--     (Started params:_) ->
--       if x >= from && x <= to$
--         then return ()
--         else Left "Out of period"
--     _ -> Left "Period is not properly defined"

listEntries :: Events AbaksEvent -> Map.Map EntryId (Either () Entry)
listEntries = foldl' go mempty
  where
    go :: Map.Map EntryId (Either () Entry) -> AbaksEvent -> Map.Map EntryId (Either () Entry)
    go entries =
      \case
        Started {} -> entries
        EntryAdded x -> Map.insert x.entryId (Right x) entries
        EntryAmountChanged {} -> entries
        EntryValidated {} -> entries
        EntryCommented {} -> entries
        EntryMarkedInConflict {} -> entries
        EntryDeleted {..} -> Map.insert entryId (Left ()) entries

validEntry :: EntryId -> Events AbaksEvent -> Either ExplainedError ()
validEntry entryId events = do
  let entries = listEntries events
  case Map.lookup entryId entries of
    Nothing -> Left "Unknown entry"
    Just (Left ()) -> Left "Deleted entry"
    Just (Right _) -> Right ()

-- listEntries :: Events AbaksEvent -> Map.Map EntryId (Either () Entry)
-- listEntries = foldl' go mempty
--   where go :: Map.Map EntryId (Either () Entry) -> AbaksEvent -> Map.Map EntryId (Either () Entry)
--         go entries =
--           \case
--             Started _ -> entries
--             EntryAdded x -> Map.insert x.entryId (Right x) entries
--             EntryAmountChanged x -> Map.adjust x.entryId (\e -> e { = }) entries
--             EntryValidated x -> Map.adjust x.entryId (\e -> e { = }) entries
--             EntryCommented x -> Map.adjust x.entryId (\e -> e { = })entries
--             EntryMarkedInConflict x -> Map.adjust x.entryId (\e -> e { = })entries
--             EntryDeleted x -> Map.insert x.entryId (Left ()) entries

-- * Various utils

type CommandHandler a e = Events a -> Either e (Events a)

type Events a = [a]
