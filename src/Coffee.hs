{-# LANGUAGE TemplateHaskell #-}

module Coffee 
    ( Weight(Oz)
    , Coffee(Coffee)
    , exampleCoffee
    , name
    , weight
    , grinds
    , description ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Weight = Oz Int deriving (Eq, Show)
$(deriveJSON defaultOptions ''Weight)

data Grind = 
    Wholebean
  | Espresso
  | Moka
  | Pourover
  | Drip
  | Frenchpress
  | Aeropress
  | Coldbrew
  | Custom String deriving (Eq, Show)
$(deriveJSON defaultOptions ''Grind)
  
data Coffee = Coffee 
  { name   :: String 
  , weight :: Weight
  , grinds :: Grind 
  , description :: String } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Coffee)

exampleCoffee :: Coffee
exampleCoffee = Coffee "Yummy" (Oz 10) Moka "yummy yummy"
