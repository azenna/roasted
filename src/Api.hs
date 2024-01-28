{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds   #-}

module Api 
    ( RoastedApi 
    , coffee
    , server) where

import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Servant (ServerT,  Get, JSON, (:>) )

import Coffee (Coffee)
import Monad (RoastedMonad, connection)
import Domain (selectCoffees)
import Database.Beam.Postgres (runBeamPostgres)


type RoastedApi = "coffees" :> Get '[JSON] [Coffee]

server :: ServerT RoastedApi RoastedMonad
server = coffee

coffee :: RoastedMonad [Coffee]
coffee = do
  conn <- asks connection
  coffees <- liftIO $ runBeamPostgres conn selectCoffees
  pure coffees
