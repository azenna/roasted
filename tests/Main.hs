{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Barbies.Bare               as B
import           Control.Monad              (join)
import qualified Control.Monad.IO.Class     as MIO
import qualified Control.Monad.Reader       as MR
import qualified Control.Monad.Trans.Except as TE
import           Data.Either                (fromRight)
import qualified Data.Functor.Barbie        as B
import           Data.Functor.Identity      (Identity (Identity))
import qualified Network.HTTP.Client        as Nhc
import           Network.Wai                (Application)
import qualified Network.Wai.Handler.Warp   as Warp
import qualified Roasted.Api.Coffee         as RAC
import qualified Roasted.Config             as RC
import qualified Roasted.Domain.Coffee      as RDC
import qualified Roasted.Monad              as RM
import qualified Servant                    as S
import qualified Servant.Client             as S
import           Servant.Client             (runClientM)
import qualified Test.Hspec                 as H

api :: S.Proxy RAC.CoffeeApi
api = S.Proxy

app :: RM.Env -> Application
app env = S.serve api $ S.hoistServer api (`MR.runReaderT` env) RAC.coffeeServer

data Env = Env { clientEnv :: S.ClientEnv }

type TestHandler = MR.ReaderT Env IO

retrieveCoffees S.:<|> retrieveCoffee S.:<|> createCoffee S.:<|> updateCoffee S.:<|> deleteCoffee = S.client api

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
      H.describe "Create coffee" $ do
        H.it "Should create a coffee" $ do
          RDC.name <$> coffee `H.shouldBe` Right "Unique"

        H.it "Should try to create coffee with null name and fail" $ do
          result <-
            S.runClientM
              (createCoffee (RDC.Coffee Nothing  Nothing Nothing))
              cEnv
          result `H.shouldSatisfy` \case
              Left (S.FailureResponse _ _) -> True
              Right _ -> False

      H.describe "Retrieve coffee" $ do
       H.it "Should retrieve all coffees" $ do
         result <- S.runClientM retrieveCoffees cEnv
         result `H.shouldSatisfy` either (const False) (not . null)

       H.it "Should retrieve a coffee" $ do
         let cId = RDC.coffeeId <$> coffee
             action = do
               cId <- TE.ExceptT (pure cId)
               TE.ExceptT (S.runClientM (retrieveCoffee cId) cEnv)
         result <- TE.runExceptT action
         result `H.shouldBe` RDC.Coffee  <$> cId <*> pure "Unique" <*> pure (Just "it's working?")

      H.describe "Update coffee" $ do
        H.it "Should update a coffee" $ do
          let cId = RDC.coffeeId <$> coffee
              update = RDC.Coffee Nothing (Just "Ununique") Nothing
              action = do
                cId <- TE.ExceptT (pure cId)
                TE.ExceptT (S.runClientM (updateCoffee cId update) cEnv)
          result <- TE.runExceptT action
          result `H.shouldBe` RDC.Coffee <$> cId <*> pure "Ununique" <*> pure (Just "it's working?")

      H.sequential $ H.describe "Delete coffee" $ do

        let cId = RDC.coffeeId <$> coffee

        H.it "Should delete a coffee" $ do
          let
            action = do
              cId <- TE.ExceptT (pure cId)
              TE.ExceptT (S.runClientM (deleteCoffee cId) cEnv)
          result <- TE.runExceptT action
          result `H.shouldBe` Right S.NoContent

        H.it "Coffee should no longer exist" $ do
          let
            action = do
              cId <- TE.ExceptT (pure cId)
              TE.ExceptT (S.runClientM (retrieveCoffee cId) cEnv)
          result <- TE.runExceptT action
          result `H.shouldSatisfy` \case
              Left (S.FailureResponse _ (S.Response err404 _ _ _ )) -> True
              Right _ -> False


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
