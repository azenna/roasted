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
  conn <- asks RM.connection
  resp <- liftIO $ HSE.run (HSE.statement () RDC.retrieveCoffeesStatement) conn
  either (const $ S.throwError S.err500) pure resp

retrieveCoffee :: Int64 -> RM.RoastedMonad RDC.Coffee
retrieveCoffee coffeeId = do
  conn <- asks RM.connection
  resp <- liftIO $ HSE.run (HSE.statement coffeeId RDC.retrieveCoffeeStatement) conn
  either (const $ S.throwError S.err500) (maybe (S.throwError S.err404) pure) resp

createCoffee :: RDC.CoffeeReq -> RM.RoastedMonad RDC.Coffee
createCoffee coffee = do
  conn <- asks RM.connection
  values <- maybe (S.throwError S.err400) pure $ RDC.parseCoffeeReq coffee 
  resp <- liftIO $ HSE.run (HSE.statement values RDC.createCoffeeStatement) conn
  either (const $ S.throwError S.err500) pure resp

updateCoffee :: Int64 -> RDC.CoffeeReq -> RM.RoastedMonad RDC.Coffee
updateCoffee coffeeId update = do
  conn <- asks RM.connection
  coffee <- retrieveCoffee coffeeId

  let new = B.bstrip $ B.bzipWith (\m d -> maybe d pure m) update (B.bcover coffee)

  resp <- liftIO $ HSE.run (HSE.statement (coffeeId, new) RDC.updateCoffeeStatement) conn
  either (const $ S.throwError S.err500) pure resp

deleteCoffee :: Int64 -> RM.RoastedMonad S.NoContent
deleteCoffee coffeeId = do
  conn <- asks RM.connection
  resp <- liftIO $ HSE.run (HSE.statement coffeeId RDC.deleteCoffeeStatement) conn
  either (const $ S.throwError S.err500) (const $ pure S.NoContent) resp
