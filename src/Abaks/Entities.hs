{-# OPTIONS_GHC -Wno-partial-fields #-}

module Abaks.Entities
  ( AbaksEvent (..),
    PeriodId (..),
    Entry (..),
    EntryId (..),
    Amount (..),
    EntryState (..),

    -- * CommandHandlers
    SemanticError (..),
    startPeriod,
    addEntry,
    changeAmountEntry,
    validateEntry,
    commentEntry,
    markInConflictEntry,
    deleteEntry,
  )
where

import Abaks.Utils.EventSourcing
import Control.Monad (unless)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

-- * Base types

newtype PeriodId = PeriodId {getPeriodId :: AggregateId}
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

startPeriod :: PeriodId -> Text -> Day -> Day -> Amount -> CommandHandler AbaksEvent SemanticError
startPeriod periodId name from to balance events = do
  unless (null events) $
    Left $
      PreconditionError "Period already started"
  return
    [ Started
        { periodId = periodId,
          name = name,
          from = from,
          to = to,
          initialBalance = balance
        }
    ]

addEntry :: Entry -> CommandHandler AbaksEvent SemanticError
addEntry entry events = do
  hasStarted events
  inPeriod entry.date events
  let entries = listEntries events
  unless (Map.notMember entry.entryId entries) $
    Left $
      PreconditionError "Entry already existing"
  return [EntryAdded entry]

changeAmountEntry :: EntryId -> Amount -> CommandHandler AbaksEvent SemanticError
changeAmountEntry entryId amount events = do
  validEntry entryId events
  return [EntryAmountChanged entryId amount]

validateEntry :: EntryId -> CommandHandler AbaksEvent SemanticError
validateEntry entryId events = do
  validEntry entryId events
  return [EntryValidated entryId]

commentEntry :: EntryId -> Text -> CommandHandler AbaksEvent SemanticError
commentEntry entryId comment events = do
  validEntry entryId events
  return [EntryCommented entryId comment]

markInConflictEntry :: EntryId -> Text -> CommandHandler AbaksEvent SemanticError
markInConflictEntry entryId reason events = do
  validEntry entryId events
  return [EntryMarkedInConflict entryId reason]

deleteEntry :: EntryId -> Text -> CommandHandler AbaksEvent SemanticError
deleteEntry entryId comment events = do
  validEntry entryId events
  return [EntryDeleted entryId comment]

-- * Various command handler utils

data SemanticError
  = MissingError Text
  | DisappearedError Text
  | InvalidError Text
  | BrokenInvariantError Text
  | PreconditionError Text
  deriving stock (Eq, Ord, Show, Generic)

hasStarted :: Events AbaksEvent -> Either SemanticError ()
hasStarted =
  \case
    (Started {} : _) -> return ()
    _ -> Left $ PreconditionError "Period is not properly defined"

inPeriod :: Day -> Events AbaksEvent -> Either SemanticError ()
inPeriod x =
  \case
    (Started {..} : _) ->
      if x >= from && x <= to
        then return ()
        else Left $ InvalidError "Out of period"
    _ -> Left $ BrokenInvariantError "Period is not properly defined"

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

validEntry :: EntryId -> Events AbaksEvent -> Either SemanticError ()
validEntry entryId events = do
  let entries = listEntries events
  case Map.lookup entryId entries of
    Nothing -> Left $ MissingError "Unknown entry"
    Just (Left ()) -> Left $ DisappearedError "Deleted entry"
    Just (Right _) -> Right ()
