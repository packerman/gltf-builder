module Core.Encode (encodeScene) where

import qualified Core.Model as Model
import qualified Gltf.Array as Array
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf

encodeScene :: Model.Scene -> Gltf
encodeScene _ =
  Gltf
    { asset = Gltf.defaultAsset,
      scene = Just 0,
      scenes =
        Array.fromList
          [ Gltf.Scene
              { name = Nothing,
                nodes = Just [0]
              }
          ],
      accessors = Array.fromList [],
      buffers = Array.fromList [],
      bufferViews = Array.fromList [],
      images = Array.fromList [],
      materials = Array.fromList [],
      meshes = Array.fromList [],
      nodes = Array.fromList [],
      samplers = Array.fromList [],
      textures = Array.fromList []
    }
