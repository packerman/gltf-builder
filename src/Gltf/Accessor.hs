module Gltf.Accessor
  ( AccessorData,
    decodeAccessorData,
    encodeAccessorData,
    fromV3List,
  )
where

import Data.Binary (Get, Put)
import Data.Binary.Get (getFloatle, runGetOrFail)
import Data.Binary.Put (putFloatle, runPut)
import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector, replicateM)
import qualified Data.Vector as V
import Linear (V3 (..))

newtype AccessorData = Vec3Float (Vector (V3 Float))
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
getAttributeData count "VEC3" 5126 = Vec3Float <$> get3dArray count getFloatle
getAttributeData _ _ _ = fail "Unknown accessor and component type"

getV3 :: Get a -> Get (V3 a)
getV3 g = V3 <$> g <*> g <*> g

get3dArray :: Int -> Get a -> Get (Vector (V3 a))
get3dArray count = replicateM count . getV3

encodeAccessorData :: AccessorData -> ByteString
encodeAccessorData = runPut . putAccessorData

putAccessorData :: AccessorData -> Put
putAccessorData (Vec3Float vector) = put3dArray putFloatle vector

putV3 :: (a -> Put) -> V3 a -> Put
putV3 p (V3 x y z) = p x <> p y <> p z

put3dArray :: (a -> Put) -> Vector (V3 a) -> Put
put3dArray = V.mapM_ . putV3
