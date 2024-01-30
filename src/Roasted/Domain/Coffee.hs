{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Roasted.Domain.Coffee
    ( insertCoffee
    , getCoffees
    , getCoffee 
    , updateCoffee ) where

import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam.Query qualified as BQ
import Roasted.Domain.Schema qualified as RDS
import Roasted.Domain.Util (runSQL)
import Roasted.Monad (RoastedMonad)
import Database.Beam.Backend.SQL.BeamExtensions qualified as BE

getCoffees :: RoastedMonad [RDS.Coffee]
getCoffees = do
  let coffees = BQ.all_ (RDS._coffee RDS.roastedDb)
  runSQL $ BQ.runSelectReturningList $ BQ.select coffees

getCoffee :: Int64 -> RoastedMonad (Maybe RDS.Coffee)
getCoffee = runSQL . BQ.runSelectReturningOne . BQ.lookup_ (RDS._coffee RDS.roastedDb) . RDS.mkCoffeeId

insertCoffee :: Text -> Maybe Text -> RoastedMonad RDS.Coffee
insertCoffee name description =do
    [coffee] <-
        runSQL $ BE.runInsertReturningList $ BQ.insert (RDS._coffee RDS.roastedDb) $ BQ.insertExpressions [ RDS.Coffee BQ.default_ (BQ.val_ name) (BQ.val_ description) ]
    pure coffee

updateCoffee :: RDS.Coffee -> RoastedMonad ()
updateCoffee coffee =
    runSQL $ BQ.runUpdate $ BQ.save (RDS._coffee RDS.roastedDb) coffee
