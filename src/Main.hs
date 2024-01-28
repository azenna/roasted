{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( app
    , main
    ) where

import Control.Monad.Reader (runReaderT)
import qualified Database.PostgreSQL.Simple as SQL
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy(Proxy), Handler, hoistServer, serve)
import System.Environment (lookupEnv)

import Api (RoastedApi, server)
import Monad (RoastedMonad, Env(Env))

api :: Proxy RoastedApi
api = Proxy

nt :: Env -> RoastedMonad a -> Handler a
nt s x = runReaderT x s

app :: Env -> Application
app s = serve api $ hoistServer api (nt s) server

getConnection :: IO SQL.Connection
getConnection = do
  port <- 
    maybe 5432 read
    <$> lookupEnv "ROASTED_POSTGRES_PORT"

  SQL.connect $ SQL.defaultConnectInfo
    { SQL.connectHost = "localhost"
    , SQL.connectPort = port
    , SQL.connectDatabase = "postgres"
    , SQL.connectUser = "postgres"
    , SQL.connectPassword = "password" }

main :: IO ()
main = do
  connection <- getConnection
  apiPort <- 
    maybe 8080 (read :: String -> Int)
    <$> lookupEnv "ROASTED_API_PORT"

  run apiPort $ app $ Env connection
