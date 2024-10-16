module Core.Dsl (module Core.Dsl) where

import Core.Model
import Data.Default
import Gltf.Json (Number)
import Linear (V4)

scene :: [Node] -> Scene
scene nodes =
  Scene
    { name = Nothing,
      nodes
    }

primitive :: Primitive -> Node
primitive p =
  def
    { mesh =
        pure $
          Mesh
            { name = Nothing,
              primitives = [p]
            }
    }

baseColor :: V4 Number -> Material
baseColor c =
  def
    { pbrMetallicRoughness =
        def {baseColorFactor = c}
    }

baseColorTexture :: TextureInfo -> Material
baseColorTexture t =
  def
    { pbrMetallicRoughness =
        def {baseColorTexture = pure t}
    }
