{-# LANGUAGE TemplateHaskell #-}

module Abaks.Utils.Random
  ( Random,
    randomUUID,
    runRandom,
  )
where

import Data.Kind
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Polysemy

data Random (m :: Type -> Type) a where
  RandomUUID :: Random m UUID.UUID

makeSem ''Random

runRandom :: Members '[Final IO] r => InterpreterFor Random r
runRandom =
  interpret $
    \case
      RandomUUID -> embedFinal UUID.nextRandom
