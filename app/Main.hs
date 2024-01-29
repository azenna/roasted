{-# LANGUAGE DataKinds #-}

module Main
  ( app,
    main,
  )
where

import Control.Monad.Reader qualified as Mr
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Roasted.Api qualified as R
import Roasted.Config qualified as R
import Roasted.Monad qualified as R
import Servant qualified as S

api :: S.Proxy R.Api
api = S.Proxy

app :: R.Env -> Application
app env = S.serve api $ S.hoistServer api (`Mr.runReaderT` env) R.server

main :: IO ()
main = do
  config <- R.getConfig
  env <- R.envFromConfig config

  run (R.apiPort config) $ app env
