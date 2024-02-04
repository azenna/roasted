{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Roasted.Api.Coffee
  ( CoffeeApi,
    coffeeServer,
  )
where

import qualified Barbies.Bare           as B
import qualified Data.Functor.Barbie    as B
import           Data.Int               (Int64)
import qualified Roasted.Domain.Coffee  as RDC
import qualified Roasted.Monad          as RM
import qualified Servant                as S
import Roasted.Api.Util qualified as RAU

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

type Delete =
  S.Capture "coffee_id" Int64
    S.:> S.Delete '[S.JSON] S.NoContent

type CoffeeApi =
  "coffee"
    S.:> (Retrieves S.:<|> Retrieve S.:<|> Create S.:<|> Update S.:<|> Delete)

coffeeServer :: S.ServerT CoffeeApi RM.RoastedMonad
coffeeServer = retrieveCoffees S.:<|> retrieveCoffee S.:<|> createCoffee  S.:<|> updateCoffee S.:<|> deleteCoffee

retrieveCoffees :: RM.RoastedMonad [RDC.Coffee]
retrieveCoffees = do
  resp <- RAU.runSingleStatement () RDC.retrieveCoffeesStatement 
  RAU.respOr500 resp

retrieveCoffee :: Int64 -> RM.RoastedMonad RDC.Coffee
retrieveCoffee coffeeId = do
  resp <- RAU.runSingleStatement coffeeId RDC.retrieveCoffeeStatement
  RAU.fromRespOr500 (maybe (S.throwError S.err404) pure) resp

createCoffee :: RDC.CoffeeReq -> RM.RoastedMonad RDC.Coffee
createCoffee coffee = do
  values <- maybe (S.throwError S.err400) pure $ RDC.parseCoffeeReq coffee 
  resp <- RAU.runSingleStatement values RDC.createCoffeeStatement
  RAU.respOr500 resp

updateCoffee :: Int64 -> RDC.CoffeeReq -> RM.RoastedMonad RDC.Coffee
updateCoffee coffeeId update = do
  coffee <- retrieveCoffee coffeeId

  let new = B.bstrip $ B.bzipWith (\m d -> maybe d pure m) update (B.bcover coffee)
  resp <- RAU.runSingleStatement (coffeeId, new) RDC.updateCoffeeStatement
  RAU.respOr500 resp

deleteCoffee :: Int64 -> RM.RoastedMonad S.NoContent
deleteCoffee coffeeId = do
  resp <- RAU.runSingleStatement coffeeId RDC.deleteCoffeeStatement
  RAU.fromRespOr500 (const $ pure S.NoContent) resp
