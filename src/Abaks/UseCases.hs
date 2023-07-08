module Abaks.UseCases
  ( createPeriod,
    addEntry,
    changeAmountEntry,
    validateEntry,
    commentEntry,
    markInClonflictEntry,
    deleteEntry,
  )
where

import qualified Abaks.Entities as Entities
import Abaks.EventSourcing
import Data.Bifunctor
import Data.Text (Text)
import Data.Time.Calendar (Day)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Polysemy

createPeriod ::
  Members '[EventSourceEffect Entities.AbaksEvent, Final IO] r =>
  Text ->
  Day ->
  Day ->
  Entities.Amount ->
  Sem r (Either Text Entities.PeriodId)
createPeriod name from to balance = do
  periodId <- Entities.PeriodId . AggregateId . UUID.toText <$> embedFinal UUID.nextRandom
  runCommand (Entities.startPeriod periodId name from to balance) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const periodId)

addEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.Entry ->
  Sem r (Either Text ())
addEntry periodId entry =
  runCommand (Entities.addEntry entry) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const ())

changeAmountEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Entities.Amount ->
  Sem r (Either Text ())
changeAmountEntry periodId entryId amount =
  runCommand (Entities.changeAmountEntry entryId amount) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const ())

validateEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Sem r (Either Text ())
validateEntry periodId entryId =
  runCommand (Entities.validateEntry entryId) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const ())

commentEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Text ->
  Sem r (Either Text ())
commentEntry periodId entryId comment =
  runCommand (Entities.commentEntry entryId comment) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const ())

markInClonflictEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Text ->
  Sem r (Either Text ())
markInClonflictEntry periodId entryId reason =
  runCommand (Entities.markInClonflictEntry entryId reason) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const ())

deleteEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.EntryId ->
  Text ->
  Sem r (Either Text ())
deleteEntry periodId entryId comment =
  runCommand (Entities.deleteEntry entryId comment) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const ())
