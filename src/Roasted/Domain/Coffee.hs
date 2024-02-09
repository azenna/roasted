{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Roasted.Domain.Coffee
  ( CoffeeH (Coffee),
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

data CoffeeH f = Coffee
  { coffeeId    :: f Int64,
    name        :: f Text,
    description :: f (Maybe Text)
  }
  deriving 
  (Generic,
   B.FunctorB,
   B.TraversableB,
   B.ApplicativeB,
   B.ConstraintsB
  )

deriving instance (B.AllBF Show f CoffeeH) => Show (CoffeeH f)
deriving instance (B.AllBF Eq f CoffeeH) => Eq (CoffeeH f)
deriving instance (B.AllBF A.ToJSON f CoffeeH) => A.ToJSON (CoffeeH f)
deriving instance (B.AllBF ToSchema f CoffeeH) => ToSchema (CoffeeH f)

instance A.FromJSON (CoffeeH I.Identity)

instance A.FromJSON (CoffeeH Maybe) where
    parseJSON = RDG.deserializeHkdGyro "Coffee" coffeeGyro

parseCoffeeReq :: CoffeeH Maybe -> Maybe (Text, Maybe Text)
parseCoffeeReq req = do

  n <- name req
  pure (n, join $ description req)

coffeeGyro :: RDG.Gyroed CoffeeH
coffeeGyro =
  Coffee {
    coffeeId = RDG.internal "coffeeId" (RDG.nonNullableRepr RDG.int8) coffeeId,
    name = RDG.external "name" (RDG.nonNullableRepr RDG.text) name,
    description = RDG.external "description" (RDG.nullableRepr RDG.text) description
  }

coffeeIdEncoder :: HE.Params Int64
coffeeIdEncoder = RDG.gyroEncoder $ coffeeId coffeeGyro

coffeeEncoder :: HE.Params (CoffeeH I.Identity)
coffeeEncoder = RDG.encodeHkdGyro coffeeGyro

coffeeDecoder :: HD.Row (CoffeeH I.Identity)
coffeeDecoder = RDG.decodeHkdGyro coffeeGyro

retrieveCoffeesStatement :: HS.Statement () [CoffeeH I.Identity]
retrieveCoffeesStatement = HS.Statement sql HE.noParams (HD.rowList coffeeDecoder) True
  where
    sql = "select * from coffee"

retrieveCoffeeStatement :: HS.Statement Int64 (Maybe (CoffeeH I.Identity))
retrieveCoffeeStatement = HS.Statement sql coffeeIdEncoder (HD.rowMaybe coffeeDecoder) True
  where
    sql = "select * from coffee where id = $1"

createCoffeeStatement :: HS.Statement (Text, Maybe Text) (CoffeeH I.Identity)
createCoffeeStatement = HS.Statement sql encoder (HD.singleRow coffeeDecoder) True
  where
    sql = "insert into coffee (name, description) values ($1, $2) returning *"
    encoder =
      (fst >$< HE.param (HE.nonNullable HE.text))
        <> (snd >$< HE.param (HE.nullable HE.text))

updateCoffeeStatement :: HS.Statement (Int64, CoffeeH I.Identity) (CoffeeH I.Identity)
updateCoffeeStatement = HS.Statement sql encoder (HD.singleRow coffeeDecoder) True
  where
    sql = "update coffee set name = $3, description = $4 where id = $1 returning *"
    encoder = (fst >$< coffeeIdEncoder) <> (snd >$< coffeeEncoder)

deleteCoffeeStatement :: HS.Statement Int64 ()
deleteCoffeeStatement = HS.Statement sql coffeeIdEncoder HD.noResult  True
  where
    sql = "delete from coffee where id = $1"
