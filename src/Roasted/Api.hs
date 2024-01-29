{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Roasted.Api
  ( Api,
    RoastedApi,
    CoffeeReq(CoffeeReq),
    server,
    roastedServer,
  )
where

import Control.Lens qualified as L
import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Aeson qualified as A
import Data.Swagger qualified as SW
import Data.Text (Text)
import Roasted.Coffee qualified as RCF
import Roasted.Domain qualified as RD
import Roasted.Monad (RoastedMonad)
import Servant qualified as S
import Servant.Swagger qualified as SSW
import Servant.Swagger.UI qualified as SSWU

type Api = RoastedApi S.:<|> SSWU.SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: SW.Swagger
swaggerDoc =
  SSW.toSwagger (S.Proxy :: S.Proxy RoastedApi)
    L.& SW.info . SW.title L..~ "RoastedApi"
    L.& SW.info . SW.version L..~ "2024.01.27"

server :: S.ServerT Api RoastedMonad
server = roastedServer S.:<|> SSWU.swaggerSchemaUIServerT swaggerDoc

type RoastedApi =
  "coffee"
    S.:> ( S.Get '[S.JSON] [RCF.Coffee]
           S.:<|> S.ReqBody '[S.JSON] CoffeeReq S.:> S.PostNoContent
           S.:<|> S.Capture "coffee_id" Int64 S.:> S.Get '[S.JSON] RCF.Coffee
       )

data CoffeeReq = CoffeeReq
  { name :: Text
  , description :: Maybe Text } deriving (Generic)

instance SW.ToSchema CoffeeReq
instance A.FromJSON CoffeeReq
instance A.ToJSON CoffeeReq

roastedServer :: S.ServerT RoastedApi RoastedMonad
roastedServer = RD.selectCoffees S.:<|> createCoffee S.:<|> retrieveCoffee

retrieveCoffee :: Int64 -> RoastedMonad RCF.Coffee
retrieveCoffee coffeeId = do
    mCoffee <- RD.lookupCoffee coffeeId
    case mCoffee of
      Just c -> pure c
      Nothing -> S.throwError S.err404


createCoffee :: CoffeeReq -> RoastedMonad S.NoContent
createCoffee coffee = (RD.insertCoffee <$> name <*> description) coffee >> pure S.NoContent
