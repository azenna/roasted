{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Coffee 
    ( Weight(Oz)
    , Coffee(Coffee)
    , exampleCoffee
    , exampleCoffeeInsertSession
    , insertCoffee
    , name
    -- , weight
    -- , grinds
    , mDescription
    , selectCoffees ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as Session

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
  , mDescription :: Maybe Text } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Coffee)

coffeeParams :: E.Params Coffee
coffeeParams = 
  (name >$< E.param (E.nonNullable E.text))
  <> (mDescription >$< E.param (E.nullable E.text))

insertCoffee :: Statement Coffee Int64
insertCoffee = Statement sql coffeeParams D.rowsAffected True where
    sql = "insert into coffee values ($1, $2)"

selectCoffees :: Statement () [Coffee]
selectCoffees = Statement sql E.noParams (D.rowList
    (Coffee
    <$> D.column (D.nonNullable D.text)
    <*> D.column (D.nullable D.text)))
    True
    where sql = "select * from coffee"

exampleCoffee :: Coffee
exampleCoffee = Coffee "Yummy" (pure "yummy yummy")

exampleCoffeeInsertSession :: Session Int64
exampleCoffeeInsertSession = Session.statement exampleCoffee insertCoffee
