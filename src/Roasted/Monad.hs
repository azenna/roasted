module Roasted.Monad
  ( Env (Env),
    RoastedMonad,
    connection,
    envFromConfig,
  )
where

import Control.Monad.Reader (ReaderT)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as SQL
import qualified Roasted.Config as RC
import Servant (Handler)

data Env = Env {connection :: Connection}

envFromConfig :: RC.Config -> IO Env
envFromConfig config = do
  conn <-
    SQL.connect $
      SQL.defaultConnectInfo
        { SQL.connectHost = "localhost",
          SQL.connectPort = (fromIntegral $ RC.postgresPort config),
          SQL.connectDatabase = "postgres",
          SQL.connectUser = "postgres",
          SQL.connectPassword = "password"
        }
  pure $ Env conn

type RoastedMonad = ReaderT Env Handler
