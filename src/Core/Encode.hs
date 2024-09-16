module Core.Encode (encodeScene) where

import Control.Monad
import Core.Model (Attribute (..), AttributeData (..), IndexData (..), Mode (..))
import qualified Core.Model as Model
import Gltf.Accessor (AccessorData (..))
import qualified Gltf.Array as Array
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf
import Gltf.Primitive (EncodingM)
import qualified Gltf.Primitive as GltfPrimitive (encodePrimitive)
import Gltf.Primitive.Types (EncodedPrimitive (..))
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

encodeMesh :: Model.Mesh -> EncodingM Gltf.Mesh
encodeMesh
  ( Model.Mesh
      { name,
        primitives
      }
    ) = do
    encodedPrimitives <- forM primitives encodePrimitive
    return $
      Gltf.Mesh
        { name,
          primitives = encodedPrimitives
        }
    where
      encodePrimitive :: Model.Primitive -> EncodingM Gltf.Primitive
      encodePrimitive
        ( Model.Primitive
            { attributes,
              indices,
              material,
              mode
            }
          ) = do
          (EncodedPrimitive {attributes = encodedAttributes, indices = encodedIndices}) <-
            GltfPrimitive.encodePrimitive
              (mapPairs encodeAttribute encodeAttributeData attributes)
              (encodeIndexData <$> indices)
          return $
            Gltf.Primitive
              { attributes = encodedAttributes,
                indices = encodedIndices,
                material = Nothing,
                mode = pure $ encodeMode mode
              }

      encodeAttribute Position = "POSITION"
      encodeAttribute (TexCoord n) = "TEXCOORD_" <> show n
      encodeAttribute Normal = "NORMAL"

      encodeAttributeData (Vec2Attribute xs) = Vec2Float xs
      encodeAttributeData (Vec3Attribute xs) = Vec3Float xs

      encodeIndexData (ShortIndex xs) = ScalarShort xs

      encodeMode Points = 0
      encodeMode Lines = 1
      encodeMode LineLoop = 2
      encodeMode LineStrip = 3
      encodeMode Triangles = 4
      encodeMode TriangleStrip = 5
      encodeMode TriangleFan = 6
