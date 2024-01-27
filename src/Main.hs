{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( startApp
    , app
    , main
    ) where

import Coffee (Coffee, exampleCoffee)

import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Server, serve, Get, JSON, Proxy(Proxy), (:>))

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

type API = "coffee" :> Get '[JSON] Coffee

startApp :: Int -> IO ()
startApp = flip run app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return exampleCoffee

main :: IO ()
main = do 
  apiPort <- 
    maybe 8080 read
    <$> lookupEnv "ROASTED_API_PORT"

  postgresPort <- 
    maybe 5432 read
    <$> lookupEnv "ROASTED_POSTGRES_PORT"
  
  Right connection <- Connection.acquire $
    Connection.settings "localhost" postgresPort "postgres" "password" "postgres"
  startApp apiPort
