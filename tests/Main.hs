{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (hspec, describe, it, shouldBe, around)
import Control.Monad.Reader (runReaderT, ReaderT, asks, ask)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication, Port)
import Servant (Proxy(Proxy), hoistServer, serve, (:<|>)(..), NoContent(NoContent))
import qualified Servant.Client as SC
import Test.Hspec (Spec, runIO)

import Roasted.Api (RoastedApi, roastedServer)
import Roasted.Config (apiPort, getConfig)
import Roasted.Monad (Env, envFromConfig)
import Roasted.Coffee (CoffeeT(Coffee))

api :: Proxy RoastedApi
api = Proxy

app :: Env -> Application
app env = serve api $ hoistServer api (`runReaderT` env) roastedServer

type TestHandler = ReaderT Application IO

allCoffee :<|> insertCoffee = SC.client api

coffee :: TestHandler Spec
coffee = do
    withApp <- asks (testWithApplication . pure)

    pure $ around withApp $ do
      let roasted = SC.client api

      baseUrl <- runIO $ SC.parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings

      let clientEnv port = SC.mkClientEnv manager (baseUrl { SC.baseUrlPort = port })

      describe "POST /coffee" $ do
          it "should create a tasty coffee" $ \port -> do
            result <- SC.runClientM (insertCoffee $ Coffee "Unique" (Just "it's working?")) (clientEnv port)
            result `shouldBe` (Right NoContent)


main :: IO ()
main = do
  config <- getConfig
  env <- envFromConfig config

  spec <- runReaderT coffee (app env)

  hspec spec
