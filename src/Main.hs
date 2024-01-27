{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( app
    , main
    ) where
import Control.Monad.Reader (runReaderT)
import qualified Hasql.Connection as Connection
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy(Proxy), Handler, hoistServer, serve)
import System.Environment (lookupEnv)

import Api (RoastedApi, server)
import Data.Word (Word16)
import Monad (RoastedMonad, Env(Env))

api :: Proxy RoastedApi
api = Proxy

nt :: Env -> RoastedMonad a -> Handler a
nt s x = runReaderT x s

app :: Env -> Application
app s = serve api $ hoistServer api (nt s) server

main :: IO ()
main = do 
  apiPort <- 
    maybe 8080 (read :: String -> Int)
    <$> lookupEnv "ROASTED_API_PORT"

  postgresPort <- 
    maybe 5432 (read :: String -> Int)
    <$> lookupEnv "ROASTED_POSTGRES_PORT"

  Right connection <- Connection.acquire $
    Connection.settings "localhost" (fromIntegral postgresPort :: Word16) "postgres" "password" "postgres"

  run apiPort $ app $ Env apiPort postgresPort connection
