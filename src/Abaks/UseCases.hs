module Abaks.UseCases
  ( createPeriod,
    addEntry,
    changeAmountEntry,
    validateEntry,
    commentEntry,
    markInConflictEntry,
    deleteEntry,
  )
where

import qualified Abaks.Entities as Entities
import Abaks.Utils.EventSourcing
import Abaks.Utils.Random
import Control.Monad (void)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import qualified Data.UUID as UUID
import Polysemy

createPeriod ::
  Members '[EventSourceEffect Entities.AbaksEvent, Random] r =>
  Text ->
  Day ->
  Day ->
  Entities.Amount ->
  Sem r (Either Entities.SemanticError Entities.PeriodId)
createPeriod name from to initialBalance = do
  periodId <- Entities.PeriodId . AggregateId . UUID.toText <$> randomUUID
  runCommand (Entities.startPeriod periodId name from to initialBalance) periodId.getPeriodId $
    return . fmap (const periodId)

addEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.Entry ->
  Sem r (Either Entities.SemanticError ())
addEntry periodId entry =
  runCommand (Entities.addEntry entry) periodId.getPeriodId $
    return . void

changeAmountEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Entities.Amount ->
  Sem r (Either Entities.SemanticError ())
changeAmountEntry periodId entryId amount =
  runCommand (Entities.changeAmountEntry entryId amount) periodId.getPeriodId $
    return . void

validateEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Sem r (Either Entities.SemanticError ())
validateEntry periodId entryId =
  runCommand (Entities.validateEntry entryId) periodId.getPeriodId $
    return . void

commentEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Text ->
  Sem r (Either Entities.SemanticError ())
commentEntry periodId entryId comment =
  runCommand (Entities.commentEntry entryId comment) periodId.getPeriodId $
    return . void

markInConflictEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Text ->
  Sem r (Either Entities.SemanticError ())
markInConflictEntry periodId entryId reason =
  runCommand (Entities.markInConflictEntry entryId reason) periodId.getPeriodId $
    return . void

deleteEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Text ->
  Sem r (Either Entities.SemanticError ())
deleteEntry periodId entryId comment =
  runCommand (Entities.deleteEntry entryId comment) periodId.getPeriodId $
    return . void
