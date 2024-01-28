module Roasted.Config (Config(Config), apiPort, postgresPort, getConfig) where

import System.Environment (lookupEnv)

data Config = Config 
  { apiPort :: Int 
  , postgresPort :: Int }

getConfig :: IO Config
getConfig = do
  api <- 
    maybe 8080 read
    <$> lookupEnv "ROASTED_API_PORT"

  postgres <- 
    maybe 5432 read
    <$> lookupEnv "ROASTED_POSTGRES_PORT"

  pure $ Config api postgres
