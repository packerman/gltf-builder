module Data.Attribute (
    decodeAttributeData,
    AttributeData,
    fromV3List,
    encodeAttributeData) where

import Data.Vector (Vector, replicateM)
import qualified Data.Vector as V
import Linear (V3(..))
import Data.ByteString.Lazy (ByteString)
import Data.Binary (Get, Put)
import Data.Binary.Get (runGetOrFail, getFloatle)
import Data.Binary.Put (runPut, putFloatle)

newtype AttributeData = Vec3Attribute (Vector (V3 Float))
    deriving (Eq, Show)

fromV3List :: [V3 Float] -> AttributeData
fromV3List = Vec3Attribute . V.fromList

type AccessorType = String
type ComponentType = Int

decodeAttributeData :: Int -> AccessorType -> ComponentType -> ByteString -> Either String AttributeData
decodeAttributeData count accessorType componentType byteString =
    case runGetOrFail (getAttributeData count accessorType componentType) byteString of
        Left (rest, consumedSize, msg) -> Left $ "Error: " <> msg <> " (consumed size = " <> show consumedSize <>
                                            ", rest = " <> show rest <> ")"
        Right (_, _, val) -> Right val

getAttributeData :: Int -> AccessorType -> ComponentType -> Get AttributeData
getAttributeData count "VEC3" 5126 = Vec3Attribute <$> get3dArray count getFloatle
getAttributeData _ _ _ = fail "Unknown accessor and component type"

getV3 :: Get a -> Get (V3 a)
getV3 g = V3 <$> g <*> g <*> g

get3dArray :: Int -> Get a -> Get (Vector (V3 a))
get3dArray count = replicateM count . getV3

encodeAttributeData :: AttributeData -> ByteString
encodeAttributeData = runPut . putAttributeData

putAttributeData :: AttributeData -> Put
putAttributeData (Vec3Attribute vector) = put3dArray putFloatle vector

putV3 :: (a -> Put) -> V3 a -> Put
putV3 p (V3 x y z) = p x <> p y <> p z

put3dArray :: (a -> Put) -> Vector (V3 a) -> Put
put3dArray = V.mapM_ . putV3
