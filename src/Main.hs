{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds   #-}

module Main
    ( startApp
    , app
    , main
    ) where

import Coffee (Coffee, exampleCoffee)

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Server, serve, Get, JSON, Proxy(Proxy), (:>))

type API = "coffee" :> Get '[JSON] Coffee

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return exampleCoffee

main :: IO ()
main = startApp
