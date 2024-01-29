{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Roasted.Coffee (CoffeeT (Coffee), Coffee, CoffeeId)where

import Data.Int (Int64)
import Data.Aeson qualified as A
import Data.Functor.Identity (Identity)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.Beam qualified as B
import GHC.Generics (Generic)


type CoffeeId = Int64

data CoffeeT f = Coffee
  { 
    id :: B.Columnar f CoffeeId,
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

type CoffeeUpdate = CoffeeT Maybe

deriving instance Show CoffeeUpdate

instance A.FromJSON CoffeeUpdate

instance B.Table CoffeeT where
  data PrimaryKey CoffeeT f = CoffeeId (B.Columnar f Text)
    deriving (Generic)
    deriving anyclass (B.Beamable)
  primaryKey = CoffeeId . name
