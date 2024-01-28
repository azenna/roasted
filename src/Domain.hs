{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain (RoastedDb, selectCoffees, insertCoffee) where

import qualified Database.Beam as B
import qualified Database.Beam.Query as BQ
import Database.Beam.Postgres (Postgres)
import GHC.Generics (Generic)

import Coffee (CoffeeT, Coffee)

data RoastedDb f = RoastedDb
  { _coffee :: f (B.TableEntity CoffeeT) }
  deriving (Generic)
  deriving anyclass (B.Database Postgres)

roastedDb :: B.DatabaseSettings be RoastedDb
roastedDb = B.defaultDbSettings

selectCoffees :: (B.MonadBeam Postgres m) => m [Coffee]
selectCoffees = do
  let coffees = BQ.all_ (_coffee roastedDb)
  BQ.runSelectReturningList $ BQ.select coffees

insertCoffee :: (B.MonadBeam Postgres m) => Coffee -> m ()
insertCoffee coffee =
  BQ.runInsert $ BQ.insert (_coffee roastedDb) (BQ.insertValues $ pure coffee)
