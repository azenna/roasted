module Monad 
  ( Env(Env)
  , RoastedMonad
  , connection ) where

import Database.PostgreSQL.Simple (Connection)
import  Control.Monad.Reader  (ReaderT)
import Servant (Handler)

data Env = Env { connection :: Connection }

type RoastedMonad = ReaderT Env Handler
