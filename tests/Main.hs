import Test.Hspec (hspec, describe, it, shouldBe)

import Control.Monad.Reader (runReaderT, ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication, Port)
import Servant (Proxy(Proxy), hoistServer, serve)
import Test.Hspec (Spec)

import Roasted.Api (RoastedApi, roastedServer)
import Roasted.Config (apiPort, getConfig)
import Roasted.Monad (Env, envFromConfig)

api :: Proxy RoastedApi
api = Proxy

app :: Env -> Application
app env = serve api $ hoistServer api (`runReaderT` env) roastedServer

type TestHandler = ReaderT Application IO

withUserApp :: (Port -> IO ()) -> TestHandler ()
withUserApp action = do
  app <- ask
  liftIO $ testWithApplication (pure app) action

main :: IO ()
main = do
  config <- getConfig
  env <- envFromConfig config

  pure ()
