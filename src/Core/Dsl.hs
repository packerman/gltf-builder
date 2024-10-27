module Core.Dsl (module Core.Dsl) where

import Core.Model
import Data.Default
import Data.Maybe (fromMaybe)
import Geometry (Geometry (makePrimitive))
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

geometry :: Geometry -> Material -> Node
geometry geom mat =
  primitive $
    fromMaybe (error "Cannot create geometry") $
      makePrimitive geom mat

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
