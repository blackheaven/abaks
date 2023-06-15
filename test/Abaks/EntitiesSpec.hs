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
      applyCommands [startPeriod anyPeriodId periodName periodStart periodEnd (Amount 0)]
        `shouldSatisfy` liftEq ($) [isRight]
    it "deleteEntry should fail" $
      applyCommands [deleteEntry (EntryId 42) "do not exist"]
        `shouldSatisfy` liftEq ($) [isLeft]

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
