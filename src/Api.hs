{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}

module Api 
    ( Api
    , RoastedApi 
    , server) where

import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((&), (.~))
import Data.Swagger (Swagger, info, version, title)
import Database.Beam.Postgres (runBeamPostgres)
import Servant (ServerT,  Get, JSON, (:>), (:<|>)(..), PostNoContent, ReqBody, NoContent (NoContent), Proxy(Proxy))
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (swaggerSchemaUIServerT, SwaggerSchemaUI)

import Coffee (Coffee)
import Monad (RoastedMonad, connection)
import Domain (selectCoffees, insertCoffee)

type Api = RoastedApi :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy RoastedApi)
    & info.title       .~ "RoastedApi"
    & info.version     .~ "2024.01.27"

server :: ServerT Api RoastedMonad
server = roastedServer :<|> swaggerSchemaUIServerT swaggerDoc

type RoastedApi = "coffee" :> 
    (Get '[JSON] [Coffee]
    :<|> ReqBody '[JSON] Coffee :> PostNoContent )

roastedServer :: ServerT RoastedApi RoastedMonad
roastedServer = coffeeAll :<|> coffeeInsert

coffeeInsert :: Coffee -> RoastedMonad NoContent
coffeeInsert coffee = do
  conn <- asks connection
  liftIO $ runBeamPostgres conn $ insertCoffee coffee
  pure NoContent

coffeeAll :: RoastedMonad [Coffee]
coffeeAll = do
  conn <- asks connection
  liftIO $ runBeamPostgres conn selectCoffees
