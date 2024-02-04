{-# LANGUAGE OverloadedStrings #-}

module Roasted.Monad
  ( Env (Env),
    RoastedMonad,
    connection,
    envFromConfig,
  )
where

import Control.Monad.Reader (ReaderT)
import Hasql.Connection as HC
import Roasted.Config qualified as RC
import Servant (Handler)

data Env = Env {connection :: Connection}

envFromConfig :: RC.Config -> IO Env
envFromConfig config = do
  Right conn <- HC.acquire $ HC.settings "localhost" (fromIntegral $ RC.postgresPort config) "postgres" "password" "postgres"
  pure $ Env conn

type RoastedMonad = ReaderT Env Handler
