{-# LANGUAGE DataKinds   #-}

module Main
    ( app
    , main
    ) where

import Control.Monad.Reader (runReaderT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy(Proxy), hoistServer, serve)

import Roasted.Api (Api, server)
import Roasted.Config (apiPort, getConfig)
import Roasted.Monad (Env, envFromConfig)

api :: Proxy Api
api = Proxy

app :: Env -> Application
app env = serve api $ hoistServer api (`runReaderT` env) server

main :: IO ()
main = do
  config <- getConfig
  env <- envFromConfig config

  run (apiPort config) $ app env
