module Core.Encode
  ( encodeScene,
    encodeSceneWithOptions,
  )
where

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
import Data.Foldable (toList)
import Data.Map (Map)
import Gltf.Accessor (AccessorData (..))
import qualified Gltf.Array as Array
import Gltf.Encode.Primitive (EncodingM)
import qualified Gltf.Encode.Primitive as GltfPrimitive (encodePrimitive)
import Gltf.Encode.Types
  ( BufferCreate (..),
    EncodedPrimitive (..),
    EncodingOptions (..),
    EncodingState (..),
    defaultEncodingOptions,
    fromMaterial,
    initialEncoding,
    setMaterialIndex,
    withBuffer,
  )
import qualified Gltf.Encode.Types as MeshPart (MeshPart (..))
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf
import Lib.Container (indexList, lookupAll, mapPairs)
import Lib.UniqueList (UniqueList)
import qualified Lib.UniqueList as UniqueList

encodeScene :: Model.Scene -> Gltf
encodeScene = encodeSceneWithOptions defaultEncodingOptions

encodeSceneWithOptions :: EncodingOptions -> Model.Scene -> Gltf
encodeSceneWithOptions encodingOptions scene@(Model.Scene {nodes, name = sceneName}) =
  let meshIndex = UniqueList.fromList $ sceneMeshes scene
      (encodedMeshes, meshPart) = encodeMeshes encodingOptions $ UniqueList.toList meshIndex
      ( MeshPart.MeshPart
          { buffers,
            accessors = encodedAccessors,
            bufferViews,
            materials
          }
        ) = meshPart
      nodeList = Model.sceneNodes scene
      nodeIndex = indexList nodeList
      encodedNodes = encodeNodes meshIndex nodeIndex nodeList
   in Gltf
        { asset = Gltf.defaultAsset,
          scene = Just 0,
          scenes =
            Array.fromList
              [ Gltf.Scene
                  { name = sceneName,
                    nodes = pure $ lookupAll nodes nodeIndex
                  }
              ],
          accessors = Array.fromList encodedAccessors,
          buffers = Array.fromList buffers,
          bufferViews = Array.fromList bufferViews,
          images = Array.fromList [],
          materials = Array.fromList materials,
          meshes = Array.fromList encodedMeshes,
          nodes = Array.fromList encodedNodes,
          samplers = Array.fromList [],
          textures = Array.fromList []
        }
  where
    encodeNodes :: UniqueList Model.Mesh -> Map Model.Node Int -> [Model.Node] -> [Gltf.Node]
    encodeNodes meshIndex nodeIndex = map encodeNode
      where
        encodeNode :: Model.Node -> Gltf.Node
        encodeNode (Model.Node {matrix, name, mesh, children}) =
          ( Gltf.Node
              { name,
                matrix = pure $ concatMap toList matrix,
                mesh = mesh >>= (`UniqueList.indexOf` meshIndex),
                children = pure $ lookupAll children nodeIndex
              }
          )

encodeMeshes :: EncodingOptions -> [Model.Mesh] -> ([Gltf.Mesh], MeshPart.MeshPart)
encodeMeshes encodingOptions meshes = evalRWS meshAction encodingOptions initialEncoding
  where
    meshAction = case bufferCreate encodingOptions of
      SingleBuffer -> do withBuffer $ forM meshes encodeMesh
      OnePerMesh -> forM meshes (withBuffer . encodeMesh)

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
            { name = materialName,
              pbrMetallicRoughness =
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
                  { name = materialName,
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
