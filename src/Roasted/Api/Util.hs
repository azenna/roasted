module Roasted.Api.Util
  ( fromRespOr500
  , respOr500
  , runSingleStatement
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Hasql.Session          as HSE
import           Hasql.Statement        as HS
import qualified Roasted.Monad          as RM
import qualified Servant                as S

fromRespOr500 :: (a -> RM.RoastedMonad b) -> Either HSE.QueryError a -> RM.RoastedMonad b
fromRespOr500 = either (const $ S.throwError S.err500)

respOr500 :: Either HSE.QueryError a -> RM.RoastedMonad a
respOr500 = fromRespOr500 pure

runSingleStatement :: a -> HS.Statement a b -> RM.RoastedMonad (Either HSE.QueryError b)
runSingleStatement a stmt = do
    conn <- asks RM.connection
    liftIO $ HSE.run (HSE.statement a stmt) conn
