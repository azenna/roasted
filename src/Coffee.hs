{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Coffee ( CoffeeT, Coffee, exampleCoffee ) where

import qualified Database.Beam as B
import GHC.Generics (Generic)
import Data.Functor.Identity (Identity)

import Data.Text (Text)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Backend (MonadBeam)
import Data.Aeson (ToJSON)

data CoffeeT f = Coffee
  { _name :: B.Columnar f Text
  , _description :: B.Columnar f (Maybe Text) }
  deriving (Generic)
  deriving anyclass (B.Beamable)

type Coffee = CoffeeT Identity
deriving instance Show Coffee

instance ToJSON Coffee

exampleCoffee :: Coffee
exampleCoffee = Coffee "Test" (pure "test test test")

instance B.Table CoffeeT where
  data PrimaryKey CoffeeT f = CoffeeId (B.Columnar f Text)
    deriving (Generic)
    deriving anyclass (B.Beamable)
  primaryKey = CoffeeId . _name
