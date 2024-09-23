module Gltf.Decode.Accessor
  ( decodeAccessorData,
    DecodeOptions (..),
  )
where

import Data.Binary (Get)
import Data.Binary.Get (getFloatle, getWord16le, runGetOrFail, skip)
import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector, replicateM)
import Gltf.Accessor (AccessorData (..))
import Linear (V2 (..), V3 (..))

type AccessorType = String

type ComponentType = Int

data DecodeOptions = DecodeOptions
  { count :: Int,
    accessorType :: AccessorType,
    componentType :: ComponentType,
    byteOffset :: Int
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
decodeAttributeData (DecodeOptions {count, accessorType, componentType, byteOffset}) =
  skip byteOffset >> getAttributeData count accessorType componentType

getAttributeData :: Int -> AccessorType -> ComponentType -> Get AccessorData
getAttributeData count "VEC3" 5126 = Vec3Float <$> getVec3Array count getFloatle
getAttributeData count "VEC2" 5126 = Vec2Float <$> getVec2Array count getFloatle
getAttributeData count "SCALAR" 5123 = ScalarShort <$> getScalarArray count getWord16le
getAttributeData _ at ct = fail $ unwords ["Unknown accessor", show at, "and component type", show ct]

getV3 :: Get a -> Get (V3 a)
getV3 g = V3 <$> g <*> g <*> g

getV2 :: Get a -> Get (V2 a)
getV2 g = V2 <$> g <*> g

getScalarArray :: Int -> Get a -> Get (Vector a)
getScalarArray = replicateM

getVec3Array :: Int -> Get a -> Get (Vector (V3 a))
getVec3Array count = replicateM count . getV3

getVec2Array :: Int -> Get a -> Get (Vector (V2 a))
getVec2Array count = replicateM count . getV2
