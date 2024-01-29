{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Roasted.Domain (RoastedDb, selectCoffees, insertCoffee, lookupCoffee) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam qualified as B
import Database.Beam.Postgres qualified as BP
import Database.Beam.Query qualified as BQ
import GHC.Generics (Generic)
import Roasted.Monad as RM
import Roasted.Coffee qualified as RCF


data RoastedDb f = RoastedDb
  {_coffee :: f (B.TableEntity RCF.CoffeeT)}
  deriving (Generic)
  deriving anyclass (B.Database BP.Postgres)

roastedDb :: B.DatabaseSettings be RoastedDb
roastedDb = B.defaultDbSettings

runSQL :: BP.Pg a -> RM.RoastedMonad a
runSQL sql = do
  conn <- asks connection
  liftIO $ BP.runBeamPostgres conn sql

selectCoffees :: RoastedMonad [RCF.Coffee]
selectCoffees = do
  let coffees = BQ.all_ (_coffee roastedDb)
  runSQL $ BQ.runSelectReturningList $ BQ.select coffees

lookupCoffee :: Int64 -> RoastedMonad (Maybe RCF.Coffee)
lookupCoffee = runSQL . BQ.runSelectReturningOne . BQ.lookup_ (_coffee roastedDb) . RCF.mkCoffeeId

insertCoffee :: Text -> Maybe Text -> RoastedMonad ()
insertCoffee name description =
  runSQL $ BQ.runInsert $ BQ.insert (_coffee roastedDb) $ BQ.insertExpressions [ RCF.Coffee B.default_ (BQ.val_ name) (BQ.val_ description) ]
