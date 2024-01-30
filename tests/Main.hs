{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class qualified as Mio
import Control.Monad.Reader qualified as Mr
import Data.Either (fromRight)
import Network.HTTP.Client qualified as Nhc
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai (Application)
import Roasted.Api.Coffee qualified as RAC
import Roasted.Domain.Schema qualified as RDS
import Roasted.Config qualified as RC
import Roasted.Monad qualified as RM
import Servant qualified as S
import Servant.Client qualified as S
import Test.Hspec qualified as H

api :: S.Proxy RAC.CoffeeApi
api = S.Proxy

app :: RM.Env -> Application
app env = S.serve api $ S.hoistServer api (`Mr.runReaderT` env) RAC.coffeeServer

type TestHandler = Mr.ReaderT Application IO

retrieveCoffees S.:<|> createCoffee S.:<|> retrieveCoffee = S.client api

coffee :: TestHandler H.Spec
coffee = do
  withApp <- Mr.asks (Warp.testWithApplication . pure)

  pure $ H.around withApp $ do
    let roasted = S.client api

    baseUrl <- H.runIO $ S.parseBaseUrl "http://localhost"
    manager <- H.runIO $ Nhc.newManager Nhc.defaultManagerSettings

    let clientEnv port = S.mkClientEnv manager (baseUrl {S.baseUrlPort = port})
        req = RAC.CoffeeReq "Unique" (Just "it's working?")

    H.describe "POST /coffee" $ do
      H.it "Should create a coffee" $ \port -> do
        result <- S.runClientM (createCoffee req) (clientEnv port)
        RDS.name <$> result `H.shouldBe` Right "Unique"

    H.describe "GET /coffee" $ do
      H.it "Should fetch all coffees" $ \port -> do
        result <- S.runClientM retrieveCoffees (clientEnv port)
        result `H.shouldSatisfy` fromRight False . fmap ((>=1) . length)

main :: IO ()
main = do
  config <- RC.getConfig
  env <- RM.envFromConfig config

  spec <- Mr.runReaderT coffee (app env)

  H.hspec spec
