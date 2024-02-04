{-# LANGUAGE DataKinds #-}

module Main
  ( app,
    main,
  )
where

import qualified Control.Monad.Reader     as MR
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import qualified Roasted.Api.Api          as RA
import qualified Roasted.Config           as RC
import qualified Roasted.Monad            as RM
import qualified Servant                  as S

api :: S.Proxy RA.Api
api = S.Proxy

app :: RM.Env -> Application
app env = S.serve api $ S.hoistServer api (`MR.runReaderT` env) RA.server

main :: IO ()
main = do
  config <- RC.getConfig
  env <- RM.envFromConfig config

  run (RC.apiPort config) $ app env
