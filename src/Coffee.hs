{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Coffee 
    ( Weight(Oz)
    , Coffee(Coffee)
    , exampleCoffee
    , name
    -- , weight
    -- , grinds
    , description ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

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
  { name   :: Text 
  , description :: Text } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Coffee)

coffeeParams :: E.Params Coffee
coffeeParams = 
  ((name) >$< E.param (E.nonNullable E.text))
  <> ((description) >$< E.param (E.nonNullable E.text))


insertCoffee :: Statement Coffee Int64
insertCoffee = Statement sql coffeeParams D.rowsAffected True where
    sql = "insert into coffee values ($1, $2)"

exampleCoffee :: Coffee
exampleCoffee = Coffee "Yummy" "yummy yummy"
