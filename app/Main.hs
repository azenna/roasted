{-# LANGUAGE DataKinds #-}

module Main
  ( app,
    main,
  )
where

import Control.Monad.Reader qualified as MR
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Roasted.Api.Api qualified as RA
import Roasted.Config qualified as RC
import Roasted.Monad qualified as RM
import Servant qualified as S

api :: S.Proxy RA.Api
api = S.Proxy

app :: RM.Env -> Application
app env = S.serve api $ S.hoistServer api (`MR.runReaderT` env) RA.server

main :: IO ()
main = do
  config <- RC.getConfig
  env <- RM.envFromConfig config

  run (RC.apiPort config) $ app env
