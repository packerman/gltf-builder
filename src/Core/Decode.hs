{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Decode (decodeScene) where

import Control.Monad ((>=>))
import Core.Model as Model
import Core.Translate.Enums
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Either.Extra (maybeToEither)
import Data.Maybe
import Data.Vector (Vector, (!?))
import Gltf.Accessor (AccessorData (..))
import Gltf.Array (Array, toVector)
import Gltf.Decode
import Gltf.Json
  ( Gltf,
    Index,
    Material (pbrMetallicRoughness),
    Number,
    TextureInfo (..),
    defaultDoubleSided,
  )
import qualified Gltf.Json as Gltf
  ( Gltf (..),
    Image,
    Material (..),
    Mesh (..),
    Node (..),
    PbrMetallicRoughness (..),
    Primitive (..),
    Sampler (..),
    Scene (..),
    Texture (..),
    TextureInfo (..),
    defaultPbrMetallicRoughness,
  )
import Lib.Container (mapPairsM)
import Linear (M44, V4 (V4), identity)

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene
  index
  ( Gltf.Gltf
      { buffers = gltfBuffers,
        bufferViews = gltfBufferViews,
        accessors = gltfAcceccors,
        materials = gltfMaterials,
        meshes = gltfMeshes,
        nodes = gltfNodes,
        scenes = gltfScenes,
        images = gltfImages,
        textures = gltfTextures,
        samplers = gltfSamplers
      }
    ) = do
    (Gltf.Scene sceneName sceneNodes) <- getIndexed gltfScenes index "scene"
    buffers <- traverse (fmap BSL.fromStrict . decodeBuffer) $ toVector gltfBuffers
    let bufferViews = toVector gltfBufferViews
    accessorData <- traverse (decodeAccessor buffers bufferViews) $ toVector gltfAcceccors
    samplers <- traverse decodeSampler $ toVector gltfSamplers
    images <- traverse decodeImage $ toVector gltfImages
    textures <- traverse (decodeTexture images samplers) $ toVector gltfTextures
    materials <- traverse (decodeMaterial textures) $ toVector gltfMaterials
    meshes <- traverse (decodeMesh accessorData materials) $ toVector gltfMeshes
    let nodeVector = toVector gltfNodes
    nodes <- traverse (decodeNode nodeVector meshes) nodeVector
    scene sceneName <$> getByIndices nodes "node" (fromMaybe [] sceneNodes)

decodeNode :: Vector Gltf.Node -> Vector Model.Mesh -> Gltf.Node -> Either String Node
decodeNode nodes meshes (Gltf.Node {name, matrix = gltfMatrix, mesh = meshIndex, children = gltfChildren}) = do
  matrix <- decodeMatrix gltfMatrix
  mesh <- traverse (getByIndex meshes "mesh") meshIndex
  gltfChildren <- getByIndices nodes "node" $ fromMaybe [] gltfChildren
  children <- traverse (decodeNode nodes meshes) gltfChildren
  return Node {..}
  where
    decodeMatrix :: Maybe [Number] -> Either String (M44 Double)
    decodeMatrix (Just [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]) =
      pure $
        V4
          (V4 a b c d)
          (V4 e f g h)
          (V4 i j k l)
          (V4 m n o p)
    decodeMatrix Nothing = pure identity
    decodeMatrix _ = Left "Incorrect matrix: expected 16 numbers"

decodeMesh :: Vector AccessorData -> Vector Model.Material -> Gltf.Mesh -> Either String Mesh
decodeMesh accessorData materials (Gltf.Mesh name primitives) = Mesh name <$> traverse decodePrimitive primitives
  where
    decodePrimitive :: Gltf.Primitive -> Either String Model.Primitive
    decodePrimitive
      ( Gltf.Primitive
          { attributes,
            indices,
            material,
            mode
          }
        ) =
        Model.Primitive
          <$> mapPairsM
            decodeAttribute
            (getByIndex accessorData "accessor" >=> decodeAttributeData)
            attributes
          <*> traverse (getByIndex accessorData "accessor" >=> decodeIndexData) indices
          <*> maybe (Right def) (getByIndex materials "material") material
          <*> decodeMode (fromMaybe 4 mode)

    decodeAttributeData accessorData = case accessorData of
      (Vec3Float xs) -> pure $ vec3Attribute xs
      (Vec2Float xs) -> pure $ vec2Attribute xs
      _ -> Left $ unwords ["Unsupported attribute accessor data:", show accessorData]

    decodeIndexData accessorData = case accessorData of
      (ScalarShort xs) -> Right $ shortIndex xs
      _ -> Left $ unwords ["Unsupported index accessor data:", show accessorData]

decodeMaterial :: Vector Model.Texture -> Gltf.Material -> Either String Model.Material
decodeMaterial textures (Gltf.Material {..}) = do
  pbrMetallicRoughness <-
    decodePbrUnsafe
      (maybe Gltf.defaultPbrMetallicRoughness (<> Gltf.defaultPbrMetallicRoughness) pbrMetallicRoughness)
  alpha <- decodeAlpha alphaMode alphaCutoff
  doubleSided <- pure $ fromMaybe defaultDoubleSided doubleSided
  return Model.Material {..}
  where
    decodePbrUnsafe (Gltf.PbrMetallicRoughness {..}) = do
      baseColorFactor <- decodeV4 $ fromJust baseColorFactor
      baseColorTexture <- traverse (decodeTextureInfo textures) baseColorTexture
      metallicRoughnessTexture <- traverse (decodeTextureInfo textures) metallicRoughnessTexture
      return
        Model.PbrMetallicRoughness
          { baseColorFactor,
            baseColorTexture,
            metallicFactor = fromJust metallicFactor,
            roughnessFactor = fromJust roughnessFactor,
            metallicRoughnessTexture
          }

    decodeTextureInfo :: Vector Model.Texture -> Gltf.TextureInfo -> Either String Model.TextureInfo
    decodeTextureInfo textures (Gltf.TextureInfo {index, texCoord}) = do
      texture <- getByIndex textures "texture" index
      return $ Model.TextureInfo texture (fromMaybe 0 texCoord)

decodeTexture :: Vector Model.Image -> Vector Model.Sampler -> Gltf.Texture -> Either String Model.Texture
decodeTexture
  images
  samplers
  ( Gltf.Texture
      { name,
        sampler,
        source
      }
    ) =
    Model.Texture name
      <$> (maybeToEither "Source is not present" source >>= getByIndex images "image")
      <*> maybe (pure def) (getByIndex samplers "sampler") sampler

decodeImage :: Gltf.Image -> Either String Model.Image
decodeImage image = do
  DataUrl {getData, mimeType} <- decodeImageData image
  return
    Model.Image
      { name = Nothing,
        mimeType,
        imageData = getData
      }

decodeSampler :: Gltf.Sampler -> Either String Model.Sampler
decodeSampler
  ( Gltf.Sampler
      { name,
        magFilter,
        minFilter,
        wrapS,
        wrapT
      }
    ) =
    Model.Sampler name
      <$> traverse decodeMagFilter magFilter
      <*> traverse decodeMinFilter minFilter
      <*> maybe (pure Repeat) decodeWrap wrapS
      <*> maybe (pure Repeat) decodeWrap wrapT

decodeV4 :: [Double] -> Either String (V4 Double)
decodeV4 [a, b, c, d] = Right $ V4 a b c d
decodeV4 _ = Left "Expected 4 numbers"

getByIndex :: Vector a -> String -> Index -> Either String a
getByIndex v name index = getOrError name index (v !? index)

getByIndices :: Vector a -> String -> [Index] -> Either String [a]
getByIndices v name = traverse (getByIndex v name)

getIndexed :: Array a -> Int -> String -> Either String a
getIndexed array index name = getOrError name index (array >>= (!? index))

getOrError :: String -> Int -> Maybe a -> Either String a
getOrError name index = maybeToEither (indexError name index)
  where
    indexError name index = unwords [name, "at index", show index, "does not exist"]
