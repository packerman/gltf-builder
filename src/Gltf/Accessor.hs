module Gltf.Accessor
  ( AccessorData (..),
    decodeAccessorData,
    encodeAccessorData,
    fromV3List,
  )
where

import Data.Binary (Get, Put)
import Data.Binary.Get (getFloatle, getWord16le, runGetOrFail)
import Data.Binary.Put (putFloatle, putWord16le, runPut)
import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector, replicateM)
import qualified Data.Vector as V
import Data.Word (Word16)
import Linear (V2 (..), V3 (..))

data AccessorData
  = Vec3Float (Vector (V3 Float))
  | Vec2Float (Vector (V2 Float))
  | ScalarShort (Vector Word16)
  deriving (Eq, Show)

fromV3List :: [V3 Float] -> AccessorData
fromV3List = Vec3Float . V.fromList

type AccessorType = String

type ComponentType = Int

decodeAccessorData :: Int -> AccessorType -> ComponentType -> ByteString -> Either String AccessorData
decodeAccessorData count accessorType componentType byteString =
  case runGetOrFail (getAttributeData count accessorType componentType) byteString of
    Left info -> Left $ errorMessage info
    Right (_, _, val) -> Right val
  where
    errorMessage (rest, consumedSize, msg) =
      "Error: "
        <> msg
        <> " (consumed size = "
        <> show consumedSize
        <> ", rest = "
        <> show rest
        <> ")"

getAttributeData :: Int -> AccessorType -> ComponentType -> Get AccessorData
getAttributeData count "VEC3" 5126 = Vec3Float <$> getVec3Array count getFloatle
getAttributeData count "VEC2" 5126 = Vec2Float <$> getVec2Array count getFloatle
getAttributeData count "SCALAR" 5123 = ScalarShort <$> getScalarArray count getWord16le
getAttributeData _ at ct = fail $ "Unknown accessor " <> show at <> " and component type " <> show ct

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

encodeAccessorData :: AccessorData -> ByteString
encodeAccessorData = runPut . putAccessorData

putAccessorData :: AccessorData -> Put
putAccessorData (Vec3Float vector) = putVec3Array putFloatle vector
putAccessorData (Vec2Float vector) = putVec2Array putFloatle vector
putAccessorData (ScalarShort vector) = putScalarArray putWord16le vector

putV3 :: (a -> Put) -> V3 a -> Put
putV3 p (V3 x y z) = p x <> p y <> p z

putVec3Array :: (a -> Put) -> Vector (V3 a) -> Put
putVec3Array = V.mapM_ . putV3

putV2 :: (a -> Put) -> V2 a -> Put
putV2 p (V2 x y) = p x <> p y

putVec2Array :: (a -> Put) -> Vector (V2 a) -> Put
putVec2Array = V.mapM_ . putV2

putScalarArray :: (a -> Put) -> Vector a -> Put
putScalarArray = V.mapM_