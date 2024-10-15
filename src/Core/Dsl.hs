module Core.Dsl (module Core.Dsl) where

import Core.Model
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
  defaultNode
    { mesh =
        pure $
          Mesh
            { name = Nothing,
              primitives = [p]
            }
    }

baseColor :: V4 Number -> Material
baseColor c =
  defaultMaterial
    { pbrMetallicRoughness =
        defaultPbrMetallicRoughness
          { baseColorFactor = c
          }
    }

baseColorTexture :: TextureInfo -> Material
baseColorTexture t =
  defaultMaterial
    { pbrMetallicRoughness =
        defaultPbrMetallicRoughness
          { baseColorTexture = pure t
          }
    }
