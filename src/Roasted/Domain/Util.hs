{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Roasted.Domain.Util (runSQL) where

import Control.Monad.Reader qualified as MR
import Control.Monad.IO.Class (liftIO)
import Database.Beam.Postgres qualified as BP
import Roasted.Monad qualified as RM

runSQL :: BP.Pg a -> RM.RoastedMonad a
runSQL sql = do
  conn <- MR.asks RM.connection
  liftIO $ BP.runBeamPostgres conn sql
