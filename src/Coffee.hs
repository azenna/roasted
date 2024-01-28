{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Coffee ( CoffeeT, Coffee, exampleCoffee ) where

import Data.Functor.Identity (Identity)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Swagger (ToSchema)
import qualified Database.Beam as B
import GHC.Generics (Generic)

data CoffeeT f = Coffee
  { _name :: B.Columnar f Text
  , _description :: B.Columnar f (Maybe Text) }
  deriving (Generic)
  deriving anyclass (B.Beamable)

type Coffee = CoffeeT Identity
deriving instance Show Coffee

instance ToJSON Coffee
instance FromJSON Coffee
instance ToSchema Coffee

type CoffeeUpdate = CoffeeT Maybe
deriving instance Show CoffeeUpdate

instance FromJSON CoffeeUpdate

exampleCoffee :: Coffee
exampleCoffee = Coffee "Test" (pure "test test test")

instance B.Table CoffeeT where
  data PrimaryKey CoffeeT f = CoffeeId (B.Columnar f Text)
    deriving (Generic)
    deriving anyclass (B.Beamable)
  primaryKey = CoffeeId . _name
