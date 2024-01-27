{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DataKinds   #-}

module Api 
    ( RoastedApi 
    , coffee
    , server) where

import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import qualified Hasql.Session as Session
import Servant (ServerT,  Get, JSON, (:>) )

import Coffee (Coffee, exampleCoffee, selectCoffees)
import Monad (RoastedMonad, connection)


type RoastedApi = "coffees" :> Get '[JSON] [Coffee]

server :: ServerT RoastedApi RoastedMonad
server = coffee

coffee :: RoastedMonad [Coffee]
coffee = do
    conn <- asks connection
    res <- liftIO $ Session.run (Session.statement () selectCoffees) conn
    case res of
      Left _ -> (pure . pure) exampleCoffee
      Right a -> pure a
