module Abaks.InterfaceAdapters.API
  ( API,
    server,
  )
where

import qualified Abaks.Entities as Entities
import qualified Abaks.UseCases as UC
import Abaks.Utils.EventSourcing
import Abaks.Utils.Random
import Abaks.Utils.Servant
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Calendar (Day)
import GHC.Generics
import Polysemy
import Polysemy.Error
import Servant.API
import Servant.Server

type API = NamedRoutes API'

data API' r = API
  { createPeriodAPI :: r :- CreatePeriodAPI,
    createEntryAPI :: r :- CreateEntryAPI,
    changeAmountEntryAPI :: r :- ChangeAmountEntryAPI,
    validateEntryAPI :: r :- ValidateEntryAPI,
    markInConflictEntryAPI :: r :- MarkInConflictEntryAPI,
    commentEntryAPI :: r :- CommentEntryAPI,
    deleteEntryAPI :: r :- DeleteEntryAPI
  }
  deriving stock (Generic)

type ApiEffects = '[EventSourceEffect Entities.AbaksEvent, Random, Error ServerError]

server :: Members ApiEffects r => ServerT API (Sem r)
server =
  API
    { createPeriodAPI = createPeriodHandler,
      createEntryAPI = createEntryHandler,
      changeAmountEntryAPI = changeAmountEntryHandler,
      validateEntryAPI = validateEntryHandler,
      markInConflictEntryAPI = markInConflictEntryHandler,
      commentEntryAPI = commentEntryHandler,
      deleteEntryAPI = deleteEntryHandler
    }

type CreatePeriodAPI =
  Summary "Create a Period"
    :> OperationId "createPeriod"
    :> ReqBody '[JSON] CreatePeriodRequest
    :> Post '[JSON] CreatePeriodResponse

data CreatePeriodRequest = CreatePeriodRequest
  { name :: Text,
    from :: Day,
    to :: Day,
    initialBalance :: AmountA
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype CreatePeriodResponse = CreatePeriodResponse
  { periodId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

createPeriodHandler ::
  Members ApiEffects r =>
  CreatePeriodRequest ->
  Sem r CreatePeriodResponse
createPeriodHandler req =
  genericUseCaseHandler (CreatePeriodResponse . (.getPeriodId.getAggregateId)) $
    UC.createPeriod req.name req.from req.to $
      toAmount req.initialBalance

type CreateEntryAPI =
  Summary "Create an Entry"
    :> OperationId "createEntry"
    :> Capture "periodId" Text
    :> ReqBody '[JSON] CreateEntryRequest
    :> Post '[JSON] NoContent

data CreateEntryRequest = CreateEntryRequest
  { entryId :: Int,
    amount :: AmountA,
    category :: Text,
    comment :: Text,
    state :: EntryStateA,
    date :: Day
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

createEntryHandler ::
  Members ApiEffects r =>
  Text ->
  CreateEntryRequest ->
  Sem r NoContent
createEntryHandler periodId req =
  genericUseCaseHandler_ $
    UC.addEntry (toPeriodId periodId) $
      Entities.Entry
        { entryId = toEntryId req.entryId,
          amount = toAmount req.amount,
          category = req.category,
          comment = req.comment,
          state = toEntryState req.state,
          date = req.date
        }

type ValidateEntryAPI =
  Summary "Validate an Entry"
    :> OperationId "validateEntry"
    :> Capture "periodId" Text
    :> "entry"
    :> Capture "entryId" Int
    :> "validate"
    :> Put '[JSON] NoContent

validateEntryHandler ::
  Members ApiEffects r =>
  Text ->
  Int ->
  Sem r NoContent
validateEntryHandler periodId entryId =
  genericUseCaseHandler_ $
    UC.validateEntry (toPeriodId periodId) (toEntryId entryId)

type MarkInConflictEntryAPI =
  Summary "Mark an Entry in conflit"
    :> OperationId "markInConflictEntry"
    :> Capture "periodId" Text
    :> "entry"
    :> Capture "entryId" Int
    :> "markInConflict"
    :> ReqBody '[JSON] MarkInConflictEntryRequest
    :> Put '[JSON] NoContent

newtype MarkInConflictEntryRequest = MarkInConflictEntryRequest
  { comment :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

markInConflictEntryHandler ::
  Members ApiEffects r =>
  Text ->
  Int ->
  MarkInConflictEntryRequest ->
  Sem r NoContent
markInConflictEntryHandler periodId entryId req =
  genericUseCaseHandler_ $
    UC.markInConflictEntry (toPeriodId periodId) (toEntryId entryId) req.comment

type CommentEntryAPI =
  Summary "Comment an Entry"
    :> OperationId "commentEntry"
    :> Capture "periodId" Text
    :> "entry"
    :> Capture "entryId" Int
    :> "comment"
    :> ReqBody '[JSON] CommentEntryRequest
    :> Put '[JSON] NoContent

newtype CommentEntryRequest = CommentEntryRequest
  { comment :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

commentEntryHandler ::
  Members ApiEffects r =>
  Text ->
  Int ->
  CommentEntryRequest ->
  Sem r NoContent
commentEntryHandler periodId entryId req =
  genericUseCaseHandler_ $
    UC.commentEntry (toPeriodId periodId) (toEntryId entryId) req.comment

type ChangeAmountEntryAPI =
  Summary "ChangeAmount an Entry"
    :> OperationId "changeAmountEntry"
    :> Capture "periodId" Text
    :> "entry"
    :> Capture "entryId" Int
    :> "amount"
    :> ReqBody '[JSON] ChangeAmountEntryRequest
    :> Put '[JSON] NoContent

newtype ChangeAmountEntryRequest = ChangeAmountEntryRequest
  { amount :: AmountA
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

changeAmountEntryHandler ::
  Members ApiEffects r =>
  Text ->
  Int ->
  ChangeAmountEntryRequest ->
  Sem r NoContent
changeAmountEntryHandler periodId entryId req =
  genericUseCaseHandler_ $
    UC.changeAmountEntry (toPeriodId periodId) (toEntryId entryId) (toAmount req.amount)

type DeleteEntryAPI =
  Summary "Delete an Entry"
    :> OperationId "deleteEntry"
    :> Capture "periodId" Text
    :> "entry"
    :> Capture "entryId" Int
    :> ReqBody '[JSON] DeleteEntryRequest
    :> Put '[JSON] NoContent

newtype DeleteEntryRequest = DeleteEntryRequest
  { comment :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

deleteEntryHandler ::
  Members ApiEffects r =>
  Text ->
  Int ->
  DeleteEntryRequest ->
  Sem r NoContent
deleteEntryHandler periodId entryId req =
  genericUseCaseHandler_ $
    UC.deleteEntry (toPeriodId periodId) (toEntryId entryId) req.comment

-- * Helpers

newtype AmountA = AmountA {cents :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

toAmount :: AmountA -> Entities.Amount
toAmount x = Entities.Amount x.cents

data EntryStateA
  = ExpectedA
  | UnexpectedA
  | ValidatedA
  | InConflictA Text
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (DerivingAPISum EntryStateA)

toEntryState :: EntryStateA -> Entities.EntryState
toEntryState =
  \case
    ExpectedA -> Entities.Expected
    UnexpectedA -> Entities.Unexpected
    ValidatedA -> Entities.Validated
    InConflictA comment -> Entities.InConflict comment

toPeriodId :: Text -> Entities.PeriodId
toPeriodId = Entities.PeriodId . AggregateId

toEntryId :: Int -> Entities.EntryId
toEntryId = Entities.EntryId

genericUseCaseHandler ::
  Members ApiEffects r =>
  (a -> b) ->
  Sem r (Either Entities.SemanticError a) ->
  Sem r b
genericUseCaseHandler onSuccess f = do
  r <- f
  let withError e msg = e {errBody = TLE.encodeUtf8 $ TL.fromStrict msg}
  case r of
    Left e ->
      throw $
        case e of
          Entities.MissingError msg -> withError err404 msg
          Entities.DisappearedError msg -> withError err410 msg
          Entities.InvalidError msg -> withError err400 msg
          Entities.BrokenInvariantError msg -> withError err500 msg
          Entities.PreconditionError msg -> withError err412 msg
    Right s -> return $ onSuccess s

genericUseCaseHandler_ ::
  Members ApiEffects r =>
  Sem r (Either Entities.SemanticError a) ->
  Sem r NoContent
genericUseCaseHandler_ = genericUseCaseHandler $ const NoContent
