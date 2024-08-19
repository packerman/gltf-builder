{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Decode (decodeScene) where

import Control.Monad ((>=>))
import Core.Model as Model
import qualified Data.ByteString.Lazy as BSL
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
    defaultPbrMetallicRoughness,
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
  )
import Linear (M44, V4 (V4), identity)
import Util.Either (maybeToEither)
import Util.Map (mapPairsM)

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
    decodeMatrix :: Maybe [Number] -> Either String (M44 Float)
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
          <*> maybe (Right Model.defaultMaterial) (getByIndex materials "material") material
          <*> decodeMode (fromMaybe 4 mode)

    decodeAttribute key = case key of
      "POSITION" -> pure Position
      "NORMAL" -> pure Normal
      "TEXCOORD_0" -> pure $ TexCoord 0
      _ -> Left $ unwords ["Unknown attribute: ", key]
    decodeAttributeData accessorData = case accessorData of
      (Vec3Float xs) -> Right $ vec3Attribute xs
      (Vec2Float xs) -> Right $ vec2Attribute xs
      _ -> Left $ unwords ["Unsupported attribute accessor data:", show accessorData]
    decodeIndexData accessorData = case accessorData of
      (ScalarShort xs) -> Right $ shortIndex xs
      _ -> Left $ unwords ["Unsupported index accessor data:", show accessorData]
    decodeMode n = case n of
      0 -> pure Points
      1 -> pure Lines
      2 -> pure LineLoop
      3 -> pure LineStrip
      4 -> pure Triangles
      5 -> pure TriangleStrip
      6 -> pure TriangleFan
      _ -> Left $ unwords ["Unkown mode:", show n]

decodeMaterial :: Vector Model.Texture -> Gltf.Material -> Either String Model.Material
decodeMaterial textures (Gltf.Material {..}) =
  Model.Material name
    <$> decodePbrUnsafe
      (maybe defaultPbrMetallicRoughness (<> defaultPbrMetallicRoughness) pbrMetallicRoughness)
  where
    decodePbrUnsafe (Gltf.PbrMetallicRoughness {..}) = do
      baseColorFactor <- decodeV4 $ fromJust baseColorFactor
      baseColorTexture <- traverse (getByIndex textures "texture" . index) baseColorTexture
      metallicRoughnessTexture <- traverse (getByIndex textures "texture" . index) metallicRoughnessTexture
      return
        Model.PbrMetallicRoughness
          { baseColorFactor,
            baseColorTexture,
            metallicFactor = fromJust metallicFactor,
            roughnessFactor = fromJust roughnessFactor,
            metallicRoughnessTexture
          }

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
      <*> maybe (pure Model.defaultSampler) (getByIndex samplers "sampler") sampler

decodeImage :: Gltf.Image -> Either String Model.Image
decodeImage image = do
  DataUrl {getData, mediaType} <- decodeImageData image
  return
    Model.Image
      { name = Nothing,
        mimeType = mediaType,
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
    where
      decodeMagFilter n = case n of
        9728 -> pure MagNearest
        9729 -> pure MagLinear
        _ -> Left $ unwords ["Unknown mag filter", show n]
      decodeMinFilter n = case n of
        9728 -> pure MinNearest
        9729 -> pure MinLinear
        9984 -> pure NearestMipMapNearest
        9985 -> pure LinearMipmapNearest
        9986 -> pure NearestMipmapLinear
        9987 -> pure LinearMipmapLinear
        _ -> Left $ unwords ["Unknown min filter", show n]
      decodeWrap n = case n of
        33071 -> pure ClampToEdge
        33648 -> pure MirroredRepeat
        10497 -> pure Repeat
        _ -> Left $ unwords ["Unknown wrap mode", show n]

decodeV4 :: [Float] -> Either String (V4 Float)
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
