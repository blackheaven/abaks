module Abaks.EntitiesSpec
  ( main,
    spec,
  )
where

import Abaks.Entities
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  return ()

applyCommands ::
  [CommandHandler AbaksEvent ExplainedError] ->
  [Either ExplainedError (Events AbaksEvent)]
applyCommands = map snd . scanl go (mempty, Left "")
  where
    go (events, _) handler =
      let result = applyCommand handler events
       in (either (const events) (events <>) result, result)
