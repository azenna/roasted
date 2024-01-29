module Roasted.Domain.Util (runSQL) where

import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Database.Beam.Postgres qualified as BP
import Roasted.Monad qualified as RM

runSQL :: BP.Pg a -> RM.RoastedMonad a
runSQL sql = do
  conn <- asks RM.connection
  liftIO $ BP.runBeamPostgres conn sql
