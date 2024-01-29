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
import Data.Aeson qualified as A
import Data.Swagger qualified as SW
import Data.Text (Text)
import Roasted.Coffee (Coffee)
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
    S.:> ( S.Get '[S.JSON] [Coffee]
           S.:<|> S.ReqBody '[S.JSON] CoffeeReq S.:> S.PostNoContent
       )

data CoffeeReq = CoffeeReq
  { name :: Text
  , description :: Maybe Text } deriving (Generic)


instance SW.ToSchema CoffeeReq
instance A.FromJSON CoffeeReq
instance A.ToJSON CoffeeReq

roastedServer :: S.ServerT RoastedApi RoastedMonad
roastedServer = coffeeAll S.:<|> coffeeInsert

coffeeInsert :: CoffeeReq -> RoastedMonad S.NoContent
coffeeInsert coffee = (RD.insertCoffee <$> name <*> description) coffee >> pure S.NoContent

coffeeAll :: RoastedMonad [Coffee]
coffeeAll = RD.selectCoffees
