{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Roasted.Api.Coffee
  ( CoffeeApi,
    coffeeServer,
  )
where

import qualified Barbies.Bare           as B
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import qualified Data.Functor.Barbie    as B
import           Data.Int               (Int64)
import           Hasql.Session          as HSE
import qualified Roasted.Domain.Coffee  as RDC
import qualified Roasted.Monad          as RM
import qualified Servant                as S

type Retrieves = S.Get '[S.JSON] [RDC.Coffee]

type Retrieve =
  S.Capture "coffee_id" Int64
    S.:> S.Get '[S.JSON] RDC.Coffee

type Create =
  S.ReqBody '[S.JSON] RDC.CoffeeReq
    S.:> S.Post '[S.JSON] RDC.Coffee

type Update =
  S.Capture "coffee_id" Int64
    S.:> S.ReqBody '[S.JSON] RDC.CoffeeReq
    S.:> S.Post '[S.JSON] RDC.Coffee

type CoffeeApi =
  "coffee"
    S.:> (Retrieves S.:<|> Retrieve S.:<|> Create S.:<|> Update )

coffeeServer :: S.ServerT CoffeeApi RM.RoastedMonad
coffeeServer = retrieveCoffees S.:<|> retrieveCoffee S.:<|> createCoffee  S.:<|> updateCoffee

retrieveCoffees :: RM.RoastedMonad [RDC.Coffee]
retrieveCoffees = do
  conn <- asks RM.connection
  resp <- liftIO $ HSE.run (HSE.statement () RDC.retrieveCoffeesStatement) conn
  case resp of
    Left _        -> S.throwError S.err500
    Right coffees -> pure coffees

retrieveCoffee :: Int64 -> RM.RoastedMonad RDC.Coffee
retrieveCoffee coffeeId = do
  conn <- asks RM.connection
  resp <- liftIO $ HSE.run (HSE.statement coffeeId RDC.retrieveCoffeeStatement) conn
  case resp of
    Right c -> pure c
    Left _  -> S.throwError S.err500

createCoffee :: RDC.CoffeeReq -> RM.RoastedMonad RDC.Coffee
createCoffee coffee = do
  conn <- asks RM.connection

  values <- case RDC.parseCoffeeReq coffee of
    Just c  -> pure c
    Nothing -> S.throwError S.err400

  resp <- liftIO $ HSE.run (HSE.statement values RDC.createCoffeeStatement) conn

  case resp of
    Left _  -> S.throwError S.err500
    Right c -> pure c

updateCoffee :: Int64 -> RDC.CoffeeReq -> RM.RoastedMonad RDC.Coffee
updateCoffee coffeeId update = do
  conn <- asks RM.connection
  coffee <- retrieveCoffee coffeeId
  let new = B.bstrip $ B.bzipWith (\m d -> maybe d pure m) update (B.bcover coffee)

  liftIO $ print new

  resp <- liftIO $ HSE.run (HSE.statement (coffeeId, new) RDC.updateCoffeeStatement) conn

  case resp of
    Left e  -> (liftIO $ print e) >> S.throwError S.err500
    Right c -> pure c
