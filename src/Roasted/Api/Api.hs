{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Roasted.Api.Api
  ( Api,
    server
  ) where

import Control.Lens qualified as L
import Data.Swagger qualified as SW
import Servant.Swagger qualified as SSW
import Servant.Swagger.UI qualified as SSWU
import Servant qualified as S
import Roasted.Api.Coffee ( CoffeeApi, coffeeServer )
import Roasted.Monad (RoastedMonad)

type Api = CoffeeApi S.:<|> SSWU.SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: SW.Swagger
swaggerDoc =
  SSW.toSwagger (S.Proxy :: S.Proxy CoffeeApi)
    L.& SW.info . SW.title L..~ "RoastedApi"
    L.& SW.info . SW.version L..~ "2024.01.27"

server :: S.ServerT Api RoastedMonad
server = coffeeServer S.:<|> SSWU.swaggerSchemaUIServerT swaggerDoc
