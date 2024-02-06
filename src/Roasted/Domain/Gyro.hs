module Roasted.Domain.Gyro
  ( Gyro(Gyro),
    decodeGyro,
    deserializeGyro,
    encodeGyro,
    encoder,
    decoder,
    nullableRepr,
    nonNullableRepr,
    internal,
    external,
    int8,
    text,
  ) where

import qualified Barbies.Bare               as B
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
greprParser (NonNullable _) key = \v -> v A..: key
greprParser (Nullable _) key    = \v -> v A..:? key

data DeserializeOpts = Internal | External

data Gyro s a = Gyro
    { gyroName        :: Text
    , grepr           :: Grepr a
    , selector        :: s -> a
    , deserializeOpts :: DeserializeOpts
    }

encoder :: Gyro s a -> HE.Params a
encoder = greprEncoder . grepr

decoder :: Gyro s a -> HD.Row a
decoder = greprDecoder . grepr

parser :: Gyro s a -> (A.Object -> Compose A.Parser Maybe a)
parser gyr = case deserializeOpts gyr of
  Internal -> \v -> Compose $ pure Nothing
  External -> Compose <$> greprParser (grepr gyr) (A.fromText (gyroName gyr))

decodeGyro :: (B.BareB h, B.FunctorB (h B.Covered), B.TraversableB (h B.Covered))
           => h B.Covered (Gyro (h B.Bare I.Identity))
           -> HD.Row (h B.Bare I.Identity)
decodeGyro hkd = B.bstrip <$> B.bsequence' (B.bmap decoder hkd)

encodeGyro :: (B.TraversableB (h B.Covered))
           => h B.Covered (Gyro (h B.Bare I.Identity))
           -> HE.Params (h B.Bare I.Identity)
encodeGyro = B.bfoldMap ((>$<) <$> selector <*> encoder)

deserializeGyro :: (B.FunctorB (h B.Covered), B.TraversableB (h B.Covered))
                => String
                -> h B.Covered (Gyro (h B.Bare I.Identity ))
                -> A.Value -> A.Parser (h B.Covered Maybe)
deserializeGyro s hkd = A.withObject s $ \v -> B.bsequence $ B.bmap (\x -> (parser x) v) hkd

internal :: Text -> Grepr a -> (s -> a) -> Gyro s a
internal s grepr sel = Gyro s grepr sel Internal

external :: Text -> Grepr a -> (s -> a) -> Gyro s a
external s grepr sel = Gyro s grepr sel External
