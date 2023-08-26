{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Abaks.Utils.Servant
  ( -- * OpenAPI descriptor
    OperationId,
    HasServer (..),
    HasClient (..),
    HasOpenApi (..),

    -- * Deriving API DTOs
    DerivingAPISum (..),
    FromJSON (..),
    ToJSON (..),
  )
where

import Data.Aeson
import Data.Function ((&))
import Data.OpenApi (allOperations, operationId)
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Lens.Micro ((?~))
import Servant.API
import Servant.Client (HasClient (..))
import Servant.OpenApi (HasOpenApi (..))
import Servant.Server
import Text.Casing (fromHumps, toKebab)

-- * OpenAPI descriptor

data OperationId (name :: Symbol)

instance HasServer subApi ctx => HasServer (OperationId name :> subApi) ctx where
  type ServerT (OperationId name :> subApi) m = ServerT subApi m
  route _ = route (Proxy @subApi)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @subApi)

instance (HasOpenApi subApi, KnownSymbol name) => HasOpenApi (OperationId name :> subApi) where
  toOpenApi _ = toOpenApi (Proxy @subApi) & allOperations . operationId ?~ apiName
    where
      apiName = T.pack $ symbolVal (Proxy @name)

instance HasClient m api => HasClient m (OperationId name :> api) where
  type Client m (OperationId name :> api) = Client m api
  clientWithRoute pm Proxy = clientWithRoute pm (Proxy :: Proxy api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy :: Proxy api)

-- * Deriving API DTOs

newtype DerivingAPISum a = DerivingAPISum {getDerivingAPISum :: a}
  deriving stock (Eq, Ord, Show, Generic)

sumtypesOptions :: Options
sumtypesOptions =
  defaultOptions
    { constructorTagModifier = toKebab . fromHumps . init,
      sumEncoding =
        TaggedObject
          { tagFieldName = "value",
            contentsFieldName = "args"
          }
    }

instance (Generic a, GToJSON' Value Zero (Rep a), GToJSON' Encoding Zero (Rep a)) => ToJSON (DerivingAPISum a) where
  toJSON = genericToJSON sumtypesOptions . getDerivingAPISum
  toEncoding = genericToEncoding sumtypesOptions . getDerivingAPISum

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (DerivingAPISum a) where
  parseJSON = fmap DerivingAPISum . genericParseJSON sumtypesOptions
