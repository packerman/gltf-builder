module Gltf.Accessor (module Gltf.Accessor) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16)
import Linear (V2, V3)

data AccessorData
  = Vec3Float (Vector (V3 Float))
  | Vec2Float (Vector (V2 Float))
  | ScalarShort (Vector Word16)
  deriving (Eq, Show)

fromV3List :: [V3 Float] -> AccessorData
fromV3List = Vec3Float . V.fromList
