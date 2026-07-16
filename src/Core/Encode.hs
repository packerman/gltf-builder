module Core.Encode
  ( encodeScene,
    encodeSceneWithOptions,
    writeScene,
    writeSceneWithOptions,
  )
where

import Control.Monad
import Control.Monad.Trans.RWS (get, modify, tell)
import Core.Model
  ( AttributeData (..),
    IndexData (..),
    sceneMeshes,
    sceneTextures,
  )
import qualified Core.Model as Model
import Core.Translate.Enums
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Gltf.Accessor (AccessorData (..))
import qualified Gltf.Array as Array
import Gltf.Delivery (Delivery (..), writeDelivery)
import Gltf.Encode.Operations (createBufferViewWithBytes, evalEncoding)
import Gltf.Encode.Primitive (EncodingM)
import qualified Gltf.Encode.Primitive as GltfPrimitive (encodePrimitive)
import Gltf.Encode.Types
  ( EncodedPrimitive (..),
    EncodingOptions (..),
    EncodingState (..),
    MeshPart,
    fromMaterial,
    isBufferImages,
    isSingleBuffer,
    setMaterialIndex,
    withBuffer,
  )
import qualified Gltf.Encode.Types as MeshPart (MeshPart (..))
import Gltf.Json (Gltf (..), defaultDoubleSided)
import qualified Gltf.Json as Gltf
import Gltf.Validate ()
import Lib.Base (nothingIf)
import Types (GltfVariant (..))
import Lib.Base64
import Lib.Container (indexList, lookupAll, mapPairs)
import Lib.MimeType (mimeTypeToString)
import Lib.UniqueList (UniqueList)
import qualified Lib.UniqueList as UL
import Linear (identity)

writeScene :: FilePath -> Model.Scene -> IO ()
writeScene = writeSceneWithOptions def

writeSceneWithOptions :: EncodingOptions -> FilePath -> Model.Scene -> IO ()
writeSceneWithOptions options filePath =
  writeDelivery filePath . encodeSceneWithOptions options

encodeScene :: Model.Scene -> Delivery
encodeScene = encodeSceneWithOptions def

type TextureIndex = UniqueList Model.Texture

data EncodedScene = EncodedScene
  { encodedImages :: [Gltf.Image],
    encodedMeshes :: [Gltf.Mesh]
  }
  deriving (Eq, Show)

data SceneIndex = SceneIndex
  { textureIndex :: UniqueList Model.Texture,
    imageIndex :: UniqueList Model.Image,
    samplerIndex :: UniqueList Model.Sampler,
    meshIndex :: UniqueList Model.Mesh
  }
  deriving (Eq, Show)

getTextureList :: SceneIndex -> [Model.Texture]
getTextureList = UL.toList . textureIndex

getMeshList :: SceneIndex -> [Model.Mesh]
getMeshList = UL.toList . meshIndex

getImageList :: SceneIndex -> [Model.Image]
getImageList = UL.toList . imageIndex

createSceneIndex :: Model.Scene -> SceneIndex
createSceneIndex scene =
  let textureIndex = UL.fromList $ sceneTextures scene
      imageIndex = UL.map Model.image textureIndex
      samplerIndex = UL.map Model.sampler textureIndex
      meshIndex = UL.fromList $ sceneMeshes scene
   in SceneIndex {..}

encodeSceneWithOptions :: EncodingOptions -> Model.Scene -> Delivery
encodeSceneWithOptions encodingOptions scene@(Model.Scene {nodes, name = sceneName}) =
  let index = createSceneIndex scene
      encodedSamplers = encodeSampler <$> UL.toList (samplerIndex index)
      encodedTextures = encodeTexture (imageIndex index) (samplerIndex index) <$> getTextureList index
      (encodedScene, meshPart) = encodeBufferRelatedParts index
      ( MeshPart.MeshPart
          { buffers,
            accessors = encodedAccessors,
            bufferViews,
            materials
          }
        ) = meshPart
      nodeList = Model.sceneNodes scene
      nodeIndex = indexList nodeList
      encodedNodes = encodeNodes (meshIndex index) nodeIndex nodeList
   in Delivery
        { deliveryUri = Nothing,
          deliveryVariant = outputVariant encodingOptions,
          deliveryJson =
            Gltf
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
                images = Array.fromList $ encodedImages encodedScene,
                materials = Array.fromList materials,
                meshes = Array.fromList $ encodedMeshes encodedScene,
                nodes = Array.fromList encodedNodes,
                samplers = Array.fromList encodedSamplers,
                textures = Array.fromList encodedTextures
              },
          deliveryBuffers =
            case outputVariant encodingOptions of
              GltfBinary ->
                V.singleton $ BSL.toStrict $ BSL.concat $ MeshPart.bytes meshPart
              _ -> V.empty,
          deliveryImages = V.empty
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

    encodeBufferRelatedParts :: SceneIndex -> (EncodedScene, MeshPart)
    encodeBufferRelatedParts index =
      let singleEncodeMod = if isSingleBuffer encodingOptions then id else withBuffer
          entireEncodeMod = if isSingleBuffer encodingOptions then withBuffer else id
       in flip evalEncoding encodingOptions $ entireEncodeMod $ do
            encodedMeshes <- traverse (singleEncodeMod . encodeMesh (textureIndex index)) $ getMeshList index
            encodedImages <- traverse (singleEncodeMod . encodeImage encodingOptions) $ getImageList index
            return EncodedScene {..}

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

      encodeTextureInfo (Model.TextureInfo {texture, texCoord}) =
        Gltf.TextureInfo
          { index = fromJust $ texture `UL.indexOf` textureIndex,
            texCoord = pure texCoord
          }

encodeImage :: EncodingOptions -> Model.Image -> EncodingM Gltf.Image
encodeImage
  encdodingOptions
  (Model.Image {name, mimeType, imageData}) =
    if isBufferImages encdodingOptions
      then do
        index <- createBufferViewWithBytes $ BS.fromStrict imageData
        return
          Gltf.Image
            { name,
              uri = Nothing,
              mimeType = Just $ mimeTypeToString mimeType,
              bufferView = Just index
            }
      else
        pure
          Gltf.Image
            { name,
              uri = pure $ encodeDataUrl $ dataUrl mimeType imageData,
              mimeType = Nothing,
              bufferView = Nothing
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

encodeTexture :: UniqueList Model.Image -> UniqueList Model.Sampler -> Model.Texture -> Gltf.Texture
encodeTexture imageIndex samplerIndex (Model.Texture {name, image, sampler}) =
  Gltf.Texture
    { name,
      sampler = sampler `UL.indexOf` samplerIndex,
      source = image `UL.indexOf` imageIndex
    }
