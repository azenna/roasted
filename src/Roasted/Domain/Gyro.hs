{-# LANGUAGE GADTs #-}

module Roasted.Domain.Gyro
  ( Gyro(Gyro),
    Gyroed,
    decodeHkdGyro,
    deserializeHkdGyro,
    encodeHkdGyro,
    gyroEncoder,
    gyroDecoder,
    nullableRepr,
    nonNullableRepr,
    internal,
    external,
    int8,
    text,
  ) where

import qualified Data.Aeson                 as A
import qualified Data.Aeson.Key             as A
import qualified Data.Aeson.Types           as A
import qualified Data.Functor.Barbie        as B
import           Data.Functor.Compose       (Compose (Compose))
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Functor.Identity      as I
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE

type Repr a = (HE.Value a, HD.Value a)

int8 :: Repr Int64
int8 = (HE.int8, HD.int8)

text :: Repr Text
text = (HE.text, HD.text)

data Grepr a where
  Nullable :: A.FromJSON a => Repr a -> Grepr (Maybe a)
  NonNullable :: A.FromJSON a => Repr a -> Grepr a

nullableRepr :: A.FromJSON a => Repr a -> Grepr (Maybe a)
nullableRepr = Nullable

nonNullableRepr :: A.FromJSON a => Repr a -> Grepr a
nonNullableRepr = NonNullable

greprEncoder :: Grepr a -> HE.Params a
greprEncoder (Nullable (enc, _))    = HE.param $ HE.nullable enc
greprEncoder (NonNullable (enc, _)) = HE.param $ HE.nonNullable enc

greprDecoder :: Grepr a -> HD.Row a
greprDecoder (Nullable (_, dec))    = HD.column $ HD.nullable dec
greprDecoder (NonNullable (_, dec)) = HD.column $ HD.nonNullable dec

greprParser :: Grepr a -> (A.Key -> A.Object -> A.Parser (Maybe a))
greprParser (NonNullable _) key = (A..: key)
greprParser (Nullable _) key    = (A..:? key)

data DeserializeOpts = Internal | External

data Gyro s a = Gyro
    { gyroName        :: Text
    , grepr           :: Grepr a
    , selector        :: forall f. s f -> f a
    , deserializeOpts :: DeserializeOpts
    }

gyroEncoder :: Gyro s a -> HE.Params a
gyroEncoder = greprEncoder . grepr

gyroDecoder :: Gyro s a -> HD.Row a
gyroDecoder = greprDecoder . grepr

gyroParser :: Gyro s a -> (A.Object -> Compose A.Parser Maybe a)
gyroParser gyr = case deserializeOpts gyr of
  Internal -> const $ Compose $ pure Nothing
  External -> Compose <$> greprParser (grepr gyr) (A.fromText (gyroName gyr))

type Gyroed h = h (Gyro h)

decodeHkdGyro :: (B.FunctorB h, B.TraversableB h)
           => Gyroed h
           -> HD.Row (h I.Identity)
decodeHkdGyro hkd = B.bsequence' (B.bmap gyroDecoder hkd)

encodeHkdGyro :: (B.TraversableB h)
           => Gyroed h
           -> HE.Params (h I.Identity)
encodeHkdGyro = B.bfoldMap ((>$<) <$> (\g -> (I.runIdentity . selector g)) <*> gyroEncoder)

deserializeHkdGyro :: (B.FunctorB h, B.TraversableB h)
                => String
                -> Gyroed h
                -> A.Value -> A.Parser (h Maybe)
deserializeHkdGyro s hkd = A.withObject s $ \v -> B.bsequence $ B.bmap (`gyroParser` v) hkd

internal :: Text -> Grepr a -> (forall f. s f -> f a) -> Gyro s a
internal s grepr sel = Gyro s grepr sel Internal

external :: Text -> Grepr a -> (forall f. s f -> f a) -> Gyro s a
external s grepr sel = Gyro s grepr sel External
