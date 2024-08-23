module Core.Encode where

import qualified Core.Model as Model
import qualified Gltf.Array as Array
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf

encodeScene :: Model.Scene -> Gltf
encodeScene scene =
  Gltf
    { asset = Gltf.defaultAsset,
      scene = Just 0,
      scenes = Array.fromList []
    }
