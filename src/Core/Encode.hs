module Core.Encode (encodeScene) where

import Control.Monad
import Control.Monad.Trans.RWS (evalRWS, get, modify, tell)
import Core.Model
  ( Attribute (..),
    AttributeData (..),
    IndexData (..),
    Mode (..),
    sceneMeshes,
  )
import qualified Core.Model as Model
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as BSL (concat, length, toStrict)
import Data.Foldable (toList)
import Gltf.Accessor (AccessorData (..))
import qualified Gltf.Array as Array
import Gltf.Json (Buffer (..), Gltf (..))
import qualified Gltf.Json as Gltf
import Gltf.Primitive (EncodingM)
import qualified Gltf.Primitive as GltfPrimitive (encodePrimitive)
import Gltf.Primitive.Types (EncodedPrimitive (..), EncodingState (..), fromMaterial, initialEncoding, setMaterialIndex)
import qualified Gltf.Primitive.Types as MeshPart (MeshPart (..))
import Lib.Base (sumWith)
import Lib.Base64 (bytesDataUrl, encodeDataUrl)
import Lib.Container (mapPairs)
import qualified Lib.UniqList as UniqList

encodeScene :: Model.Scene -> Gltf
encodeScene scene =
  let meshIndex = UniqList.fromList $ sceneMeshes scene
      (encodedMeshes, meshPart) = encodeMeshes $ UniqList.toList meshIndex
      ( MeshPart.MeshPart
          { bytes = encodedBytes,
            accessors = encodedAccessors,
            bufferViews,
            materials
          }
        ) = meshPart
   in Gltf
        { asset = Gltf.defaultAsset,
          scene = Just 0,
          scenes =
            Array.fromList
              [ Gltf.Scene
                  { name = Nothing,
                    nodes = Just [0]
                  }
              ],
          accessors = Array.fromList encodedAccessors,
          buffers = Array.fromList [encodeBuffer encodedBytes],
          bufferViews = Array.fromList bufferViews,
          images = Array.fromList [],
          materials = Array.fromList materials,
          meshes = Array.fromList encodedMeshes,
          nodes = Array.fromList [],
          samplers = Array.fromList [],
          textures = Array.fromList []
        }
  where
    encodeBuffer :: [ByteString] -> Buffer
    encodeBuffer byteStrings =
      Buffer
        { byteLength = fromIntegral $ sumWith BSL.length byteStrings,
          uri = pure $ encodeDataUrl (bytesDataUrl $ BSL.toStrict $ BSL.concat byteStrings),
          name = Nothing
        }

encodeMeshes :: [Model.Mesh] -> ([Gltf.Mesh], MeshPart.MeshPart)
encodeMeshes meshes = evalRWS (forM meshes encodeMesh) () initialEncoding

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
          encodedMaterial <- encodeMaterial material
          return $
            Gltf.Primitive
              { attributes = encodedAttributes,
                indices = encodedIndices,
                material = pure encodedMaterial,
                mode = pure $ encodeMode mode
              }

      encodeAttribute Position = "POSITION"
      encodeAttribute (TexCoord n) = "TEXCOORD_" <> show n
      encodeAttribute Normal = "NORMAL"

      encodeAttributeData (Vec2Attribute xs) = Vec2Float xs
      encodeAttributeData (Vec3Attribute xs) = Vec3Float xs

      encodeIndexData (ShortIndex xs) = ScalarShort xs

      encodeMaterial :: Model.Material -> EncodingM Int
      encodeMaterial
        ( Model.Material
            { pbrMetallicRoughness =
                Model.PbrMetallicRoughness
                  { baseColorFactor,
                    metallicFactor,
                    roughnessFactor
                  }
            }
          ) =
          do
            (EncodingState {materialIndex}) <- get
            modify $ setMaterialIndex (materialIndex + 1)
            tell $
              fromMaterial $
                Gltf.Material
                  { name,
                    pbrMetallicRoughness =
                      pure $
                        Gltf.PbrMetallicRoughness
                          { baseColorFactor = pure $ toList baseColorFactor,
                            metallicFactor = pure metallicFactor,
                            roughnessFactor = pure roughnessFactor,
                            baseColorTexture = Nothing,
                            metallicRoughnessTexture = Nothing
                          }
                  }
            return materialIndex

      encodeMode Points = 0
      encodeMode Lines = 1
      encodeMode LineLoop = 2
      encodeMode LineStrip = 3
      encodeMode Triangles = 4
      encodeMode TriangleStrip = 5
      encodeMode TriangleFan = 6
