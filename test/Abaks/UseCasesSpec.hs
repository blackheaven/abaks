module Abaks.UseCasesSpec
  ( main,
    spec,
  )
where

import Abaks.Entities (AbaksEvent, Amount (..), Entry (..), EntryId (..), EntryState (..))
import Abaks.EventSourcing
import Abaks.UseCases
import Data.Text (Text)
import Data.Time.Calendar
import Polysemy
import Polysemy.Error
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "UseCases" $
    around withFixture $ do
      it "All chained commands should work" $ \fixture ->
        runUsecase
          fixture
          ( runError @Text $ do
              periodId <- createPeriod periodName periodStart periodEnd (Amount 0) >>= fromEither
              addEntry periodId anEntry >>= fromEither
              changeAmountEntry periodId anEntry.entryId (Amount 1500) >>= fromEither
              validateEntry periodId anEntry.entryId >>= fromEither
              commentEntry periodId anEntry.entryId "The Hateful 8: too cool" >>= fromEither
              markInClonflictEntry periodId anEntry.entryId "way too expensive" >>= fromEither
              deleteEntry periodId anEntry.entryId "wrong period" >>= fromEither
          )
          `shouldReturn` Right ()

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

newtype Fixture = Fixture
  { runUsecase :: forall a. Sem '[EventSourceEffect AbaksEvent, Final IO] a -> IO a
  }

withFixture :: ActionWith Fixture -> IO ()
withFixture action = action $ Fixture $ runFinal . runMemoryUnsafe
