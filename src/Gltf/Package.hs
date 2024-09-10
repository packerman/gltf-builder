module Gltf.Package () where

import Control.Monad.Zip
import Core.Model (AttributeData)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Gltf.Accessor (AccessorData (..))
import Gltf.Json (Accessor (..), BufferView (..))
import Lib.Container (groupBy)
import Prelude
import qualified Prelude as P (max, min)

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
    createAccessor =
      Accessor
        { bufferView = undefined,
          byteOffset = undefined,
          componentType = undefined,
          count = undefined,
          name = undefined,
          accessorType = undefined,
          max = undefined,
          min = undefined
        }

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

mzipMin :: (MonadZip m, Ord a) => Vector (m a) -> m a
mzipMin = V.foldl1' (mzipWith P.min)

mzipMax :: (MonadZip m, Ord a) => Vector (m a) -> m a
mzipMax = V.foldl1' (mzipWith P.max)

foldl1Zip' :: (MonadZip m) => (a -> a -> a) -> Vector (m a) -> m a
foldl1Zip' f = V.foldl1' (mzipWith f)

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
