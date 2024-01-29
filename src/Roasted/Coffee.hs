{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Roasted.Coffee (CoffeeT (Coffee), Coffee, mkCoffeeId) where

import Data.Int (Int64)
import Data.Aeson qualified as A
import Data.Functor.Identity (Identity)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.Beam qualified as B
import GHC.Generics (Generic)

data CoffeeT f = Coffee
  { 
    coffeeId :: B.Columnar f Int64,
    name :: B.Columnar f Text,
    description :: B.Columnar f (Maybe Text)
  }
  deriving (Generic)
  deriving anyclass (B.Beamable)

type Coffee = CoffeeT Identity
deriving instance Show Coffee
instance A.ToJSON Coffee
instance A.FromJSON Coffee
instance ToSchema Coffee

instance B.Table CoffeeT where
  data PrimaryKey CoffeeT f = CoffeeId (B.Columnar f Int64)
    deriving (Generic)
    deriving anyclass (B.Beamable)
  primaryKey = CoffeeId . coffeeId

mkCoffeeId :: B.Columnar f Int64 -> B.PrimaryKey CoffeeT f
mkCoffeeId = CoffeeId
