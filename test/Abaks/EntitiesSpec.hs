module Abaks.EntitiesSpec
  ( main,
    spec,
  )
where

import Abaks.Entities
import Data.Either
import Data.Functor.Classes
import Data.List (foldl')
import Data.Text (Text)
import Data.Time.Calendar
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Entities" $ do
    it "startPeriod should work" $
      applyCommands [startingCommand]
        `shouldSatisfy` liftEq ($) [isRight]
    it "Not started should fail" $
      applyCommands
        [ addEntry anEntry,
          changeAmountEntry anEntry.entryId (Amount 1500),
          validateEntry anEntry.entryId,
          commentEntry anEntry.entryId "The Hateful 8: too cool",
          markInClonflictEntry anEntry.entryId "way too expensive",
          deleteEntry anEntry.entryId "wrong period"
        ]
        `shouldSatisfy` liftEq ($) [isLeft, isLeft, isLeft, isLeft, isLeft, isLeft]
    it "Out of period should fail" $
      applyCommands
        [ startingCommand,
          addEntry anEntry {date = fromGregorian 2022 1 15},
          addEntry anEntry {date = fromGregorian 2024 1 15}
        ]
        `shouldSatisfy` liftEq ($) [isRight, isLeft, isLeft]
    it "All chained commands should work" $
      applyCommands
        [ startingCommand,
          addEntry anEntry,
          changeAmountEntry anEntry.entryId (Amount 1500),
          validateEntry anEntry.entryId,
          commentEntry anEntry.entryId "The Hateful 8: too cool",
          markInClonflictEntry anEntry.entryId "way too expensive",
          deleteEntry anEntry.entryId "wrong period"
        ]
        `shouldSatisfy` liftEq ($) [isRight, isRight, isRight, isRight, isRight, isRight, isRight]
    it "Wrong entryId should fail" $
      applyCommands
        [ startingCommand,
          addEntry anEntry {entryId = EntryId 42},
          changeAmountEntry anEntry.entryId (Amount 1500),
          validateEntry anEntry.entryId,
          commentEntry anEntry.entryId "The Hateful 8: too cool",
          markInClonflictEntry anEntry.entryId "way too expensive",
          deleteEntry anEntry.entryId "wrong period"
        ]
        `shouldSatisfy` liftEq ($) [isRight, isRight, isLeft, isLeft, isLeft, isLeft, isLeft]
    it "No entry should fail" $
      applyCommands
        [ startingCommand,
          changeAmountEntry anEntry.entryId (Amount 1500),
          validateEntry anEntry.entryId,
          commentEntry anEntry.entryId "The Hateful 8: too cool",
          markInClonflictEntry anEntry.entryId "way too expensive",
          deleteEntry anEntry.entryId "wrong period"
        ]
        `shouldSatisfy` liftEq ($) [isRight, isLeft, isLeft, isLeft, isLeft, isLeft]
    it "Deleted entry should fail" $
      applyCommands
        [ startingCommand,
          addEntry anEntry,
          deleteEntry anEntry.entryId "wrong period",
          changeAmountEntry anEntry.entryId (Amount 1500),
          validateEntry anEntry.entryId,
          commentEntry anEntry.entryId "The Hateful 8: too cool",
          markInClonflictEntry anEntry.entryId "way too expensive",
          deleteEntry anEntry.entryId "wrong period"
        ]
        `shouldSatisfy` liftEq ($) [isRight, isRight, isRight, isLeft, isLeft, isLeft, isLeft, isLeft]

applyCommands ::
  [CommandHandler AbaksEvent ExplainedError] ->
  [Either ExplainedError (Events AbaksEvent)]
applyCommands = snd . foldl' go (mempty, mempty)
  where
    go (events, previousResults) handler =
      let result = applyCommand handler events
       in (either (const events) (events <>) result, previousResults <> [result])

anyPeriodId :: PeriodId
anyPeriodId = PeriodId 0

periodName :: Text
periodName = "Jan 2023"

periodStart :: Day
periodStart = fromGregorian 2023 1 1

periodEnd :: Day
periodEnd = fromGregorian 2023 1 31

anEntry :: Entry
anEntry =
  Entry
    { entryId = EntryId 1,
      amount = Amount 2000,
      category = "cinema",
      comment = "The Hateful 8",
      state = Unexpected,
      date = fromGregorian 2023 1 24
    }

startingCommand :: CommandHandler AbaksEvent ExplainedError
startingCommand = startPeriod anyPeriodId periodName periodStart periodEnd (Amount 0)
