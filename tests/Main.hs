{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.IO.Class qualified as MIO
import Control.Monad.Reader qualified as MR
import Data.Either (fromRight)
import Network.HTTP.Client qualified as Nhc
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Roasted.Api.Coffee qualified as RAC
import Roasted.Config qualified as RC
import Roasted.Domain.Coffee qualified as RDC
import Roasted.Monad qualified as RM
import Servant qualified as S
import Servant.Client qualified as S
import Test.Hspec qualified as H
import qualified Network.Wai.Handler.Warp as Warp

api :: S.Proxy RAC.CoffeeApi
api = S.Proxy

app :: RM.Env -> Application
app env = S.serve api $ S.hoistServer api (`MR.runReaderT` env) RAC.coffeeServer

data Env = Env { clientEnv :: S.ClientEnv }

type TestHandler = MR.ReaderT Env IO

retrieveCoffees S.:<|> retrieveCoffee S.:<|> createCoffee S.:<|> updateCoffee = S.client api

coffee :: TestHandler H.Spec
coffee = do
    cEnv <- MR.asks clientEnv

    coffee <- MR.liftIO $
      S.runClientM 
        (createCoffee 
          (RDC.Coffee 
            Nothing 
            (pure "Unique") 
            (pure $ pure "it's working?"))) 
        cEnv

    pure $ do
      H.describe "POST /coffee" $ do
        H.it "Should create a coffee" $ do
          RDC.name <$> coffee `H.shouldBe` Right "Unique"

        H.it "Should try to create coffee with null name and fail" $ \port -> do
          result <- 
            S.runClientM 
              (createCoffee (RDC.Coffee Nothing  Nothing Nothing))
              cEnv
          result `H.shouldSatisfy` \case
              Left (S.FailureResponse _ _) -> True
              Right _ -> False

      H.describe "GET /coffee" $ do
       H.it "Should fetch all coffees" $ do
         result <- S.runClientM retrieveCoffees cEnv
         result `H.shouldSatisfy` either (const False) (not . null)

main :: IO ()
main = do
  config <- RC.getConfig
  appEnv <- RM.envFromConfig config

  baseUrl <-  S.parseBaseUrl "http://localhost"
  manager <-  Nhc.newManager Nhc.defaultManagerSettings

  Warp.testWithApplication (pure $ app appEnv) $ \port -> do

    let clientEnv = S.mkClientEnv manager (baseUrl {S.baseUrlPort = port})
        testEnv = Env clientEnv

    spec <- MR.runReaderT coffee testEnv
    H.hspec spec
