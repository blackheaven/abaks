{-# LANGUAGE TemplateHaskell #-}

module Abaks.Utils.EventSourcing
  ( AggregateId (..),
    Events,
    CommandHandler,
    applyCommand,
    EventSourceEffect,
    withEvents,
    runCommand,
    runMemoryUnsafe,
    FileSettings,
    mkFileSettings,
    runFile,
  )
where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Data.Aeson
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics
import Polysemy
import Polysemy.State

-- * Entities

type CommandHandler a e = Events a -> Either e (Events a)

type Events a = [a]

-- * Use Cases

newtype AggregateId = AggregateId {getAggregateId :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data EventSourceEffect (eventType :: Type) (m :: Type -> Type) (a :: Type) where
  WithEvents :: AggregateId -> (Events eventType -> m (Events eventType, a)) -> EventSourceEffect eventType m a

makeSem ''EventSourceEffect

-- * Drivers

runMemoryUnsafe :: forall eventType r. InterpreterFor (EventSourceEffect eventType) r
runMemoryUnsafe = evalState @(Map.Map AggregateId (Events eventType)) mempty . go
  where
    go =
      reinterpretH $
        \case
          WithEvents aggregateId f -> do
            allEvents <- get @(Map.Map AggregateId (Events eventType))
            let originalAggregateEvent = Map.findWithDefault mempty aggregateId allEvents
            mf <- runTSimple $ f originalAggregateEvent
            inspector <- getInspectorT
            case inspect inspector mf of
              Nothing -> return ()
              Just (newEvents, _) -> put $ Map.insert aggregateId (originalAggregateEvent <> newEvents) allEvents
            return $ snd <$> mf

data FileSettings = FileSettings
  { fileStorePath :: FilePath,
    fileStoreLock :: MVar ()
  }

mkFileSettings :: FilePath -> IO FileSettings
mkFileSettings p = FileSettings p <$> newMVar ()

runFile ::
  forall eventType r.
  (FromJSON eventType, ToJSON eventType, Members '[Final IO] r) =>
  FileSettings ->
  InterpreterFor (EventSourceEffect eventType) r
runFile settings =
  interpretH $
    \case
      WithEvents aggregateId f -> do
        embedFinal $ takeMVar settings.fileStoreLock
        allEvents <-
          either (error . (("Error opening " <> settings.fileStorePath <> ": ") <>)) id
            <$> embedFinal (eitherDecodeFileStrict' @(Map.Map AggregateId (Events eventType)) settings.fileStorePath)
        let originalAggregateEvent = Map.findWithDefault mempty aggregateId allEvents
        mf <- runTSimple $ f originalAggregateEvent
        inspector <- getInspectorT
        case inspect inspector mf of
          Nothing -> return ()
          Just (newEvents, _) ->
            embedFinal $
              encodeFile settings.fileStorePath $
                Map.insert aggregateId (originalAggregateEvent <> newEvents) allEvents
        embedFinal $ putMVar settings.fileStoreLock ()
        return $ snd <$> mf

-- * Internals

applyCommand ::
  CommandHandler a e ->
  Events a ->
  Either e (Events a)
applyCommand = ($)

runCommand ::
  Members '[EventSourceEffect eventType] r =>
  CommandHandler eventType e ->
  AggregateId ->
  (Either e (Events eventType) -> Sem r a) ->
  Sem r a
runCommand handler aggregateId f =
  withEvents aggregateId $ \initialEvents ->
    let result = applyCommand handler initialEvents
     in (,) (either mempty id result) <$> f result
