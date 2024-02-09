{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Roasted.Api.Coffee
  ( CoffeeApi,
    coffeeServer,
  )
where

import qualified Data.Functor.Barbie   as B
import qualified Data.Functor.Identity as I
import           Data.Int              (Int64)
import qualified Roasted.Api.Util      as RAU
import qualified Roasted.Domain.Coffee as RDC
import qualified Roasted.Monad         as RM
import qualified Servant               as S

type Retrieves = S.Get '[S.JSON] [RDC.CoffeeH I.Identity]

type Retrieve =
  S.Capture "coffee_id" Int64
    S.:> S.Get '[S.JSON] (RDC.CoffeeH I.Identity)

type Create =
  S.ReqBody '[S.JSON] (RDC.CoffeeH Maybe)
    S.:> S.Post '[S.JSON] (RDC.CoffeeH I.Identity)

type Update =
  S.Capture "coffee_id" Int64
    S.:> S.ReqBody '[S.JSON] (RDC.CoffeeH Maybe)
    S.:> S.Post '[S.JSON] (RDC.CoffeeH I.Identity)

type Delete =
  S.Capture "coffee_id" Int64
    S.:> S.Delete '[S.JSON] S.NoContent

type CoffeeApi =
  "coffee"
    S.:> (Retrieves S.:<|> Retrieve S.:<|> Create S.:<|> Update S.:<|> Delete)

coffeeServer :: S.ServerT CoffeeApi RM.RoastedMonad
coffeeServer = retrieveCoffees S.:<|> retrieveCoffee S.:<|> createCoffee  S.:<|> updateCoffee S.:<|> deleteCoffee

retrieveCoffees :: RM.RoastedMonad [RDC.CoffeeH I.Identity]
retrieveCoffees = do
  resp <- RAU.runSingleStatement () RDC.retrieveCoffeesStatement
  RAU.respOr500 resp

retrieveCoffee :: Int64 -> RM.RoastedMonad (RDC.CoffeeH I.Identity)
retrieveCoffee coffeeId = do
  resp <- RAU.runSingleStatement coffeeId RDC.retrieveCoffeeStatement
  RAU.fromRespOr500 (maybe (S.throwError S.err404) pure) resp

createCoffee :: RDC.CoffeeH Maybe -> RM.RoastedMonad (RDC.CoffeeH I.Identity)
createCoffee coffee = do
  values <- maybe (S.throwError S.err400) pure $ RDC.parseCoffeeReq coffee
  resp <- RAU.runSingleStatement values RDC.createCoffeeStatement
  RAU.respOr500 resp

updateCoffee :: Int64 -> RDC.CoffeeH Maybe -> RM.RoastedMonad (RDC.CoffeeH I.Identity)
updateCoffee coffeeId update = do
  coffee <- retrieveCoffee coffeeId

  let new = B.bzipWith (\m d -> maybe d pure m) update coffee
  resp <- RAU.runSingleStatement (coffeeId, new) RDC.updateCoffeeStatement
  RAU.respOr500 resp

deleteCoffee :: Int64 -> RM.RoastedMonad S.NoContent
deleteCoffee coffeeId = do
  resp <- RAU.runSingleStatement coffeeId RDC.deleteCoffeeStatement
  RAU.fromRespOr500 (const $ pure S.NoContent) resp
