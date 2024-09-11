module Gltf.Package () where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Gltf.Accessor (AccessorData (..))
import Gltf.Json (Accessor (..), Buffer (..), BufferView (..))
import Lib.Base (mzipMax, mzipMin, nothingIf)
import Lib.Container (groupBy)
import Prelude

data Package = Package
  {
  }
  deriving (Eq, Show)

encodeAttributes :: Map String AccessorData -> Package
encodeAttributes attributes =
  let strideGroups = groupBy (stride . snd) (M.assocs attributes)
   in undefined
  where
    encodeWithStride :: Int -> [(String, AccessorData)] -> Package
    encodeWithStride groupStride attributeList = undefined
    createAccessor :: AccessorData -> Accessor
    createAccessor (Vec3Float xs) =
      Accessor
        { bufferView = undefined,
          byteOffset = undefined,
          componentType = 5126,
          count = undefined,
          name = Nothing,
          accessorType = "VEC3",
          max = pure $ toList $ mzipMax xs,
          min = pure $ toList $ mzipMin xs
        }
    createAccessor _ = error ""
    createArrayBuffer :: Int -> BufferView
    createArrayBuffer byteStride =
      BufferView
        { buffer = undefined,
          byteOffset = undefined,
          byteLength = undefined,
          byteStride = nothingIf (== 0) byteStride,
          name = Nothing,
          target = pure 34962
        }
    createElementArrayBuffer :: BufferView
    createElementArrayBuffer =
      BufferView
        { buffer = undefined,
          byteOffset = undefined,
          byteLength = undefined,
          byteStride = Nothing,
          name = Nothing,
          target = pure 34963
        }
    createBuffer :: Buffer
    createBuffer =
      Buffer
        { byteLength = undefined,
          name = Nothing,
          uri = undefined
        }

encodePrimitive :: Map String AccessorData -> Maybe AccessorData -> Package
encodePrimitive attributes indices = undefined

stride :: AccessorData -> Int
stride (Vec3Float _) = 12
stride (Vec2Float _) = 8
stride (ScalarShort _) = 2

byteSize :: AccessorData -> Int
byteSize (Vec3Float xs) = 12 * V.length xs
byteSize (Vec2Float xs) = 8 * V.length xs
byteSize (ScalarShort xs) = 2 * V.length xs

elemCount :: AccessorData -> Int
elemCount (Vec3Float xs) = V.length xs
elemCount (Vec2Float xs) = V.length xs
elemCount (ScalarShort xs) = V.length xs
