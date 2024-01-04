module Main (main) where

import Abaks.InterfaceAdapters.API
import qualified Abaks.Utils.EventSourcing as EventSourcing
import qualified Abaks.Utils.Random as Random
import Control.Monad.Except (ExceptT (..))
import Network.Wai.Handler.Warp
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Servant

main :: IO ()
main = do
  let settings = setPort 8080 $ setHost "0.0.0.0" defaultSettings
  eventSourcingSettings <- EventSourcing.mkFileSettings "abaks-aggregates.json"
  runSettings settings $
    serve (Proxy @API) $
      hoistServer (Proxy @API) (hoistHandler eventSourcingSettings) server

hoistHandler ::
  EventSourcing.FileSettings ->
  Sem (Append ApiEffects '[Final IO]) a ->
  Handler a
hoistHandler eventSourcingSettings =
  Handler
    . ExceptT
    . runFinal
    . runError
    . Random.runRandom
    . EventSourcing.runFile eventSourcingSettings
