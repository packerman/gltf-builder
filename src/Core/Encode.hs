module Core.Encode
  ( encodeScene,
    encodeSceneWithOptions,
    writeScene,
  )
where

import Control.Monad
import Control.Monad.Trans.RWS (evalRWS, get, modify, tell)
import Core.Model
  ( Alpha (..),
    Attribute (..),
    AttributeData (..),
    IndexData (..),
    MagFilter (..),
    MinFilter (..),
    Mode (..),
    Wrap (..),
    sceneMeshes,
    sceneTextures,
  )
import qualified Core.Model as Model
import Data.Default
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Gltf.Accessor (AccessorData (..))
import qualified Gltf.Array as Array
import Gltf.Encode (writeGltf)
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
import Gltf.Json (Gltf (..), defaultAlphaCutoff, defaultDoubleSided)
import qualified Gltf.Json as Gltf
import Gltf.Validate ()
import Lib.Base (nothingIf)
import Lib.Base64
import Lib.Container (indexList, lookupAll, mapPairs)
import Lib.UniqueList (UniqueList)
import qualified Lib.UniqueList as UL
import Linear (identity)

writeScene :: FilePath -> Model.Scene -> IO ()
writeScene filePath = writeGltf filePath . encodeScene

encodeScene :: Model.Scene -> Gltf
encodeScene = encodeSceneWithOptions defaultEncodingOptions

type TextureIndex = UniqueList Model.Texture

encodeSceneWithOptions :: EncodingOptions -> Model.Scene -> Gltf
encodeSceneWithOptions encodingOptions scene@(Model.Scene {nodes, name = sceneName}) =
  let textureIndex = UL.fromList $ sceneTextures scene
      imageIndex = UL.map Model.image textureIndex
      samplerIndex = UL.map Model.sampler textureIndex
      encodedImages = encodeImage <$> UL.toList imageIndex
      encodedSamplers = encodeSampler <$> UL.toList samplerIndex
      encodedTextures = encodeTexture imageIndex samplerIndex <$> UL.toList textureIndex
      meshIndex = UL.fromList $ sceneMeshes scene
      (encodedMeshes, meshPart) = encodeMeshes encodingOptions textureIndex $ UL.toList meshIndex
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
        { asset = def,
          scene = Just 0,
          scenes =
            Array.fromList
              [ Gltf.Scene
                  { name = sceneName,
                    nodes = nothingIf null $ lookupAll nodes nodeIndex
                  }
              ],
          accessors = Array.fromList encodedAccessors,
          buffers = Array.fromList buffers,
          bufferViews = Array.fromList bufferViews,
          images = Array.fromList encodedImages,
          materials = Array.fromList materials,
          meshes = Array.fromList encodedMeshes,
          nodes = Array.fromList encodedNodes,
          samplers = Array.fromList encodedSamplers,
          textures = Array.fromList encodedTextures
        }
  where
    encodeNodes :: UniqueList Model.Mesh -> Map Model.Node Int -> [Model.Node] -> [Gltf.Node]
    encodeNodes meshIndex nodeIndex = map encodeNode
      where
        encodeNode :: Model.Node -> Gltf.Node
        encodeNode (Model.Node {matrix, name, mesh, children}) =
          ( Gltf.Node
              { name,
                matrix = concatMap toList <$> nothingIf (== identity) matrix,
                mesh = mesh >>= (`UL.indexOf` meshIndex),
                children = nothingIf null $ lookupAll children nodeIndex
              }
          )

encodeMeshes :: EncodingOptions -> TextureIndex -> [Model.Mesh] -> ([Gltf.Mesh], MeshPart.MeshPart)
encodeMeshes encodingOptions textureIndex meshes = evalRWS meshAction encodingOptions initialEncoding
  where
    meshAction = case bufferCreate encodingOptions of
      SingleBuffer -> do withBuffer $ forM meshes (encodeMesh textureIndex)
      OnePerMesh -> forM meshes (withBuffer . encodeMesh textureIndex)

encodeMesh :: TextureIndex -> Model.Mesh -> EncodingM Gltf.Mesh
encodeMesh
  textureIndex
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
                    roughnessFactor,
                    baseColorTexture,
                    metallicRoughnessTexture
                  },
              alpha,
              doubleSided
            }
          ) =
          let (alphaMode, alphaCutoff) = encodeAlphaMode alpha
           in do
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
                                baseColorTexture = encodeTextureInfo <$> baseColorTexture,
                                metallicRoughnessTexture = encodeTextureInfo <$> metallicRoughnessTexture
                              },
                        alphaMode,
                        alphaCutoff,
                        doubleSided = nothingIf (== defaultDoubleSided) doubleSided
                      }
                return materialIndex

      encodeAlphaMode Opaque = (Nothing, Nothing)
      encodeAlphaMode (Mask alphaCutoff) = (pure "MASK", nothingIf (== defaultAlphaCutoff) alphaCutoff)
      encodeAlphaMode Blend = (pure "BLEND", Nothing)

      encodeMode Points = 0
      encodeMode Lines = 1
      encodeMode LineLoop = 2
      encodeMode LineStrip = 3
      encodeMode Triangles = 4
      encodeMode TriangleStrip = 5
      encodeMode TriangleFan = 6

      encodeTextureInfo (Model.TextureInfo {texture, texCoord}) =
        Gltf.TextureInfo
          { index = fromJust $ texture `UL.indexOf` textureIndex,
            texCoord = pure texCoord
          }

encodeImage :: Model.Image -> Gltf.Image
encodeImage (Model.Image {name, mimeType, imageData}) =
  Gltf.Image
    { name,
      uri = pure $ encodeDataUrl $ dataUrl mimeType imageData
    }

encodeSampler :: Model.Sampler -> Gltf.Sampler
encodeSampler
  ( Model.Sampler
      { magFilter,
        minFilter,
        wrapS,
        wrapT
      }
    ) =
    Gltf.Sampler
      { name = Nothing,
        magFilter = encodeMagFilter <$> magFilter,
        minFilter = encodeMinFilter <$> minFilter,
        wrapS = pure $ encodeWrap wrapS,
        wrapT = pure $ encodeWrap wrapT
      }
    where
      encodeMagFilter f = case f of
        MagNearest -> 9728
        MagLinear -> 9729
      encodeMinFilter f = case f of
        MinNearest -> 9728
        MinLinear -> 9729
        NearestMipMapNearest -> 9984
        LinearMipmapNearest -> 9985
        NearestMipmapLinear -> 9986
        LinearMipmapLinear -> 9987
      encodeWrap w = case w of
        ClampToEdge -> 33071
        MirroredRepeat -> 33648
        Repeat -> 10497

encodeTexture :: UniqueList Model.Image -> UniqueList Model.Sampler -> Model.Texture -> Gltf.Texture
encodeTexture imageIndex samplerIndex (Model.Texture {name, image, sampler}) =
  Gltf.Texture
    { name,
      sampler = sampler `UL.indexOf` samplerIndex,
      source = image `UL.indexOf` imageIndex
    }
