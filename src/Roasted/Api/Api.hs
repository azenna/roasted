{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Roasted.Api.Api
  ( Api,
    server,
  )
where

import qualified Control.Lens       as L
import qualified Data.Swagger       as SW
import           Roasted.Api.Coffee (CoffeeApi, coffeeServer)
import           Roasted.Monad      (RoastedMonad)
import qualified Servant            as S
import qualified Servant.Swagger    as SSW
import qualified Servant.Swagger.UI as SSWU

type Api = CoffeeApi S.:<|> SSWU.SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: SW.Swagger
swaggerDoc =
  SSW.toSwagger (S.Proxy :: S.Proxy CoffeeApi)
    L.& SW.info . SW.title L..~ "RoastedApi"
    L.& SW.info . SW.version L..~ "2024.01.27"

server :: S.ServerT Api RoastedMonad
server = coffeeServer S.:<|> SSWU.swaggerSchemaUIServerT swaggerDoc
