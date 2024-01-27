module Monad 
  ( Env(Env)
  , RoastedMonad
  , apiPort
  , connection
  , postgresPort ) where

import  Control.Monad.Reader  (ReaderT)
import Hasql.Connection (Connection)
import Servant (Handler)

data Env = Env
  { apiPort :: Int
  , postgresPort :: Int
  , connection :: Connection }

type RoastedMonad = ReaderT Env Handler
