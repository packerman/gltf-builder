module Gltf.Decode.Accessor (decodeAccessorData, AccessorData) where

import Data.Vector (Vector, (!?), replicateM)
import Linear (V3(..))
import Data.Binary (Get, Binary(..))
import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get (runGetOrFail)

newtype AccessorData = Vec3Data (Vector (V3 Float))
    deriving (Eq, Show)

type AccessorType = String
type ComponentType = Int

decodeAccessorData :: Int -> AccessorType -> ComponentType -> ByteString -> Either String AccessorData
decodeAccessorData count accessorType componentType byteString =
    case runGetOrFail (getAccessorData count accessorType componentType) byteString of
        Left (_, _, msg) -> Left msg
        Right (_, _, val) -> Right val

getAccessorData :: Int -> AccessorType -> ComponentType -> Get AccessorData
getAccessorData count "VEC3" 5126 = Vec3Data <$> get3dArray count
getAccessorData _ _ _ = fail "Unknown accessor and component type"

getV3 :: Get a -> Get (V3 a)
getV3 g = V3 <$> g <*> g <*> g

get3dArray :: (Binary a) => Int -> Get (Vector (V3 a))
get3dArray count = replicateM count get
