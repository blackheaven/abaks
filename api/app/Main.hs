module Main where

-- import Control.Concurrent.MVar
import Control.Monad
-- import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
-- import Data.OpenApi hiding (Server, delete, server, title, url)
-- import GHC.Generics
-- import Network.HTTP.Types
-- import Network.URI
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
-- import Servant.OpenApi
import Universum

main :: IO ()
main = do
  let frontCors =
        simpleCorsResourcePolicy
          { corsOrigins = Just (["http://localhost:8080", "http://localhost:4200"], True),
            corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"],
            corsRequestHeaders = ["Authorization", "Content-Type", "Navigation"]
          }
  putStrLn @Text "Serving on 8080"
  run 8080 $
    cors (const $ Just frontCors) $ serve (Proxy @API) server

type API =
  Summary "Test endpoint"
    :> Get '[JSON] Msg

server :: Server API
server = return $ Msg "It works!"

newtype Msg = Msg {msg :: String}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
