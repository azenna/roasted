module Roasted.Monad 
  ( Env(Env)
  , RoastedMonad
  , connection
  , envFromConfig ) where

import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as SQL
import  Control.Monad.Reader  (ReaderT)
import Servant (Handler)

import Roasted.Config (Config, postgresPort)

data Env = Env { connection :: Connection }

envFromConfig :: Config -> IO Env
envFromConfig config = do
      conn <- SQL.connect $ SQL.defaultConnectInfo
        { SQL.connectHost = "localhost"
        , SQL.connectPort = (fromIntegral $ postgresPort config)
        , SQL.connectDatabase = "postgres"
        , SQL.connectUser = "postgres"
        , SQL.connectPassword = "password" }
      pure $ Env conn

type RoastedMonad = ReaderT Env Handler
