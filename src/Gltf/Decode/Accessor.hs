module Gltf.Decode.Accessor
  ( decodeAccessorData,
    DecodeOptions (..),
  )
where

import Data.Binary (Get)
import Data.Binary.Get (getFloatle, getWord16le, runGetOrFail, skip)
import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector, replicateM)
import qualified Data.Vector as V
import Gltf.Accessor (AccessorData (..))
import Linear (V2 (..), V3 (..))

type AccessorType = String

type ComponentType = Int

data DecodeOptions = DecodeOptions
  { count :: Int,
    accessorType :: AccessorType,
    componentType :: ComponentType,
    byteOffset :: Int,
    byteStride :: Maybe Int
  }
  deriving (Eq, Show)

decodeAccessorData :: DecodeOptions -> ByteString -> Either String AccessorData
decodeAccessorData options byteString =
  case runGetOrFail (decodeAttributeData options) byteString of
    Left info -> Left $ errorMessage info
    Right (_, _, val) -> Right val
  where
    errorMessage (rest, consumedSize, msg) =
      unwords ["Error:", msg, "(consumed size =", show consumedSize, ", rest =", show rest, ")"]

decodeAttributeData :: DecodeOptions -> Get AccessorData
decodeAttributeData options@(DecodeOptions {byteOffset}) =
  skip byteOffset >> getAttributeData options

getAttributeData :: DecodeOptions -> Get AccessorData
getAttributeData
  ( DecodeOptions
      { count,
        accessorType,
        componentType,
        byteStride
      }
    ) = getAttributeData' accessorType componentType
    where
      getAttributeData' "VEC3" 5126 = Vec3Float <$> getVec3Array count (toSkip 12 byteStride) getFloatle
      getAttributeData' "VEC2" 5126 = Vec2Float <$> getVec2Array count (toSkip 8 byteStride) getFloatle
      getAttributeData' "SCALAR" 5123 = ScalarShort <$> getScalarArray count getWord16le
      getAttributeData' at ct = fail $ unwords ["Unknown accessor", show at, "and component type", show ct]

      toSkip :: Int -> Maybe Int -> Int
      toSkip naturalStride = maybe 0 (\bs -> bs - naturalStride)

getV3 :: Get a -> Get (V3 a)
getV3 g = V3 <$> g <*> g <*> g

getV2 :: Get a -> Get (V2 a)
getV2 g = V2 <$> g <*> g

getScalarArray :: Int -> Get a -> Get (Vector a)
getScalarArray = replicateM

getVec3Array :: Int -> Int -> Get a -> Get (Vector (V3 a))
getVec3Array count toSkip elemGet =
  let vectorGet = getV3 elemGet
   in getVectorArray count vectorGet toSkip

getVec2Array :: Int -> Int -> Get a -> Get (Vector (V2 a))
getVec2Array count toSkip elemGet =
  let vectorGet = getV2 elemGet
   in getVectorArray count vectorGet toSkip

getVectorArray :: Int -> Get a -> Int -> Get (Vector a)
getVectorArray count vectorGet toSkip
  | toSkip <= 0 = replicateM count vectorGet
  | count == 1 = V.singleton <$> vectorGet
  | otherwise = V.cons <$> vectorGet <*> replicateM (count - 1) (skip toSkip >> vectorGet)
