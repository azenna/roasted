{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class qualified as Mio
import Control.Monad.Reader qualified as Mr
import Network.HTTP.Client qualified as Nhc
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai (Application)
import Roasted.Api qualified as R
import Roasted.Config qualified as R
import Roasted.Monad qualified as R
import Servant qualified as S
import Servant.Client qualified as S
import Test.Hspec qualified as H

api :: S.Proxy R.RoastedApi
api = S.Proxy

app :: R.Env -> Application
app env = S.serve api $ S.hoistServer api (`Mr.runReaderT` env) R.roastedServer

type TestHandler = Mr.ReaderT Application IO

allCoffee S.:<|> insertCoffee = S.client api

coffee :: TestHandler H.Spec
coffee = do
  withApp <- Mr.asks (Warp.testWithApplication . pure)

  pure $ H.around withApp $ do
    let roasted = S.client api

    baseUrl <- H.runIO $ S.parseBaseUrl "http://localhost"
    manager <- H.runIO $ Nhc.newManager Nhc.defaultManagerSettings

    let clientEnv port = S.mkClientEnv manager (baseUrl {S.baseUrlPort = port})

    H.describe "POST /coffee" $ do
      H.it "should create a tasty coffee" $ \port -> do
        result <- S.runClientM (insertCoffee (R.CoffeeReq "Unique" (Just "it's working?"))) (clientEnv port)
        result `H.shouldBe` Right S.NoContent

main :: IO ()
main = do
  config <- R.getConfig
  env <- R.envFromConfig config

  spec <- Mr.runReaderT coffee (app env)

  H.hspec spec
