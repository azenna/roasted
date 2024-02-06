{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Roasted.Domain.Coffee
  ( CoffeeHT (Coffee),
    Coffee,
    CoffeeH,
    CoffeeReq,
    coffeeId,
    name,
    description,
    retrieveCoffeesStatement,
    retrieveCoffeeStatement,
    createCoffeeStatement,
    updateCoffeeStatement,
    deleteCoffeeStatement,
    parseCoffeeReq,
  )
where

import qualified Barbies.Bare               as B
import           Control.Monad              (join)
import qualified Data.Aeson                 as A
import qualified Data.Functor.Barbie        as B
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Functor.Identity      as I
import           Data.Int                   (Int64)
import           Data.Swagger               (ToSchema)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import           Hasql.Statement            as HS
import qualified Roasted.Domain.Gyro        as RDG

data CoffeeHT t f = Coffee
  { coffeeId    :: B.Wear t f Int64,
    name        :: B.Wear t f Text,
    description :: B.Wear t f (Maybe Text)
  }
  deriving (Generic)

instance B.BareB CoffeeHT

instance B.FunctorB (CoffeeHT B.Covered)

instance B.TraversableB (CoffeeHT B.Covered)

instance B.ApplicativeB (CoffeeHT B.Covered)

instance B.ConstraintsB (CoffeeHT B.Covered)

deriving instance (B.AllBF Show f (CoffeeHT B.Covered)) => Show (CoffeeHT B.Covered f)

deriving instance (B.AllBF Eq f (CoffeeHT B.Covered)) => Eq (CoffeeHT B.Covered f)

type Coffee = CoffeeHT B.Bare I.Identity

deriving instance Show Coffee
deriving instance Eq Coffee

instance ToSchema Coffee

instance A.ToJSON Coffee

instance A.FromJSON Coffee

type CoffeeReq = CoffeeHT B.Covered Maybe
--
instance ToSchema CoffeeReq
--
instance A.ToJSON CoffeeReq
--
instance A.FromJSON CoffeeReq where
  parseJSON = RDG.deserializeGyro "Coffee" coffeeGyro

parseCoffeeReq :: CoffeeReq -> Maybe (Text, Maybe Text)
parseCoffeeReq req = do
  n <- name req
  pure (n, join $ description req)

type CoffeeH f = CoffeeHT B.Covered f

coffeeGyro :: CoffeeH (RDG.Gyro Coffee)
coffeeGyro =
  Coffee {
    coffeeId = RDG.internal "coffeeId" (RDG.nonNullableRepr RDG.int8) (coffeeId :: Coffee -> Int64),
    name = RDG.external "name" (RDG.nonNullableRepr RDG.text) (name :: Coffee -> Text),
    description = RDG.external "description" (RDG.nullableRepr RDG.text) (description :: Coffee -> Maybe Text)
  }

coffeeIdEncoder :: HE.Params Int64
coffeeIdEncoder = RDG.encoder $ coffeeId coffeeGyro

coffeeEncoder :: HE.Params Coffee
coffeeEncoder = RDG.encodeGyro coffeeGyro

coffeeDecoder :: HD.Row Coffee
coffeeDecoder = RDG.decodeGyro coffeeGyro

retrieveCoffeesStatement :: HS.Statement () [Coffee]
retrieveCoffeesStatement = HS.Statement sql HE.noParams (HD.rowList coffeeDecoder) True
  where
    sql = "select * from coffee"

retrieveCoffeeStatement :: HS.Statement Int64 (Maybe Coffee)
retrieveCoffeeStatement = HS.Statement sql coffeeIdEncoder (HD.rowMaybe coffeeDecoder) True
  where
    sql = "select * from coffee where id = $1"

createCoffeeStatement :: HS.Statement (Text, Maybe Text) Coffee
createCoffeeStatement = HS.Statement sql encoder (HD.singleRow coffeeDecoder) True
  where
    sql = "insert into coffee (name, description) values ($1, $2) returning *"
    encoder =
      (fst >$< HE.param (HE.nonNullable HE.text))
        <> (snd >$< HE.param (HE.nullable HE.text))

updateCoffeeStatement :: HS.Statement (Int64, Coffee) Coffee
updateCoffeeStatement = HS.Statement sql encoder (HD.singleRow coffeeDecoder) True
  where
    sql = "update coffee set name = $3, description = $4 where id = $1 returning *"
    encoder = (fst >$< coffeeIdEncoder) <> (snd >$< coffeeEncoder)

deleteCoffeeStatement :: HS.Statement Int64 ()
deleteCoffeeStatement = HS.Statement sql coffeeIdEncoder HD.noResult  True
  where
    sql = "delete from coffee where id = $1"
