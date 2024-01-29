{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Roasted.Domain.Coffee
    ( insertCoffee
    , getCoffees
    , getCoffee ) where

import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam.Query qualified as BQ
import Roasted.Domain.Schema qualified as RDS
import Roasted.Domain.Util (runSQL)
import Roasted.Monad (RoastedMonad)

getCoffees :: RoastedMonad [RDS.Coffee]
getCoffees = do
  let coffees = BQ.all_ (RDS._coffee RDS.roastedDb)
  runSQL $ BQ.runSelectReturningList $ BQ.select coffees

getCoffee :: Int64 -> RoastedMonad (Maybe RDS.Coffee)
getCoffee = runSQL . BQ.runSelectReturningOne . BQ.lookup_ (RDS._coffee RDS.roastedDb) . RDS.mkCoffeeId

insertCoffee :: Text -> Maybe Text -> RoastedMonad ()
insertCoffee name description =
  runSQL $ BQ.runInsert $ BQ.insert (RDS._coffee RDS.roastedDb) $ BQ.insertExpressions [ RDS.Coffee BQ.default_ (BQ.val_ name) (BQ.val_ description) ]
