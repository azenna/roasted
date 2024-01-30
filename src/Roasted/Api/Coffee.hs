{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Roasted.Api.Coffee
  ( CoffeeApi,
    CoffeeReq(CoffeeReq),
    coffeeServer
  ) where

import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Aeson qualified as A
import Data.Text (Text)
import Data.Swagger (ToSchema)
import Roasted.Domain.Coffee qualified as RDC
import Roasted.Domain.Schema qualified as RDS
import Roasted.Monad (RoastedMonad)
import Servant qualified as S

type CoffeeApi =
  "coffee"
    S.:> ( S.Get '[S.JSON] [RDS.Coffee]
           S.:<|> S.ReqBody '[S.JSON] CoffeeReq S.:> S.Post '[S.JSON] RDS.Coffee
           S.:<|> S.Capture "coffee_id" Int64 S.:> S.Get '[S.JSON] RDS.Coffee
           S.:<|> S.Capture "coffee_id" Int64 S.:> S.ReqBody '[S.JSON] CoffeeReq S.:> S.Post '[S.JSON] RDS.Coffee
       )

data CoffeeReq = CoffeeReq
  { name :: Text
  , description :: Maybe Text } deriving (Generic)

instance ToSchema CoffeeReq
instance A.FromJSON CoffeeReq
instance A.ToJSON CoffeeReq

coffeeServer :: S.ServerT CoffeeApi RoastedMonad
coffeeServer = RDC.getCoffees S.:<|> createCoffee S.:<|> retrieveCoffee S.:<|> updateCoffee

retrieveCoffee :: Int64 -> RoastedMonad RDS.Coffee
retrieveCoffee coffeeId = do
    mCoffee <- RDC.getCoffee coffeeId
    case mCoffee of
      Just c -> pure c
      Nothing -> S.throwError S.err404

updateCoffee :: Int64 -> CoffeeReq -> RoastedMonad RDS.Coffee
updateCoffee coffeeId req = do
    coffee <- retrieveCoffee coffeeId
    let updated = coffee { RDS.name = name req, RDS.description = description req}
    RDC.updateCoffee updated
    pure updated


createCoffee :: CoffeeReq -> RoastedMonad RDS.Coffee
createCoffee coffee = (RDC.insertCoffee <$> name <*> description) coffee
