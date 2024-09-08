module Core.Encode (encodeScene) where

import Core.Model (Attribute (..), AttributeData (..), Mode (..))
import qualified Core.Model as Model
import Gltf.Accessor (AccessorData (..))
import qualified Gltf.Array as Array
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf
import Lib.Container (mapPairs)

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

encodeMesh :: Model.Mesh -> Gltf.Mesh
encodeMesh
  ( Model.Mesh
      { name,
        primitives
      }
    ) =
    Gltf.Mesh
      { name,
        primitives = encodePrimitive <$> primitives
      }
    where
      encodePrimitive
        ( Model.Primitive
            { attributes,
              indices,
              material,
              mode
            }
          ) =
          Gltf.Primitive
            { attributes = undefined, -- mapPairs encodeAttribute encodeAttributeData attributes,
              indices = Nothing,
              material = Nothing,
              mode = pure $ encodeMode mode
            }

      encodeAttribute Position = "POSITION"
      encodeAttribute (TexCoord n) = "TEXCOORD_" <> show n
      encodeAttribute Normal = "NORMAL"
      encodeAttributeData (Vec2Attribute xs) = Vec2Float xs
      encodeAttributeData (Vec3Attribute xs) = Vec3Float xs

      encodeMode Points = 0
      encodeMode Lines = 1
      encodeMode LineLoop = 2
      encodeMode LineStrip = 3
      encodeMode Triangles = 4
      encodeMode TriangleStrip = 5
      encodeMode TriangleFan = 6
