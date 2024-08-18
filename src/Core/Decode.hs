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
import Gltf.Json (Gltf, Index, Material (pbrMetallicRoughness), Number, defaultPbrMetallicRoughness)
import qualified Gltf.Json as Gltf (Gltf (..), Material (..), Mesh (..), Node (..), PbrMetallicRoughness (..), Primitive (..))
import qualified Gltf.Json as Scene (Scene (..))
import Linear (M44, V4 (V4), identity)
import Util.Either (maybeToEither)
import Util.Map (mapPairsM)

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene index gltf = do
  gltfScene <- getIndexed (Gltf.scenes gltf) index "scene"
  buffers <- traverse (fmap BSL.fromStrict . decodeBuffer) $ toVector $ Gltf.buffers gltf
  let bufferViews = toVector (Gltf.bufferViews gltf)
  accessorData <- traverse (decodeAccessor buffers bufferViews) $ toVector $ Gltf.accessors gltf
  materials <- traverse decodeMaterial $ toVector $ Gltf.materials gltf
  meshes <- traverse (decodeMesh accessorData materials) $ toVector $ Gltf.meshes gltf
  let gltfNodes = toVector $ Gltf.nodes gltf
  nodes <- traverse (decodeNode gltfNodes meshes) gltfNodes
  scene (Scene.name gltfScene) <$> getByIndices nodes "node" (fromMaybe [] $ Scene.nodes gltfScene)

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
      Right $
        V4
          (V4 a b c d)
          (V4 e f g h)
          (V4 i j k l)
          (V4 m n o p)
    decodeMatrix Nothing = Right identity
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
      "POSITION" -> Right Position
      "NORMAL" -> Right Normal
      "TEXCOORD_0" -> Right $ TexCoord 0
      _ -> Left $ unwords ["Unknown attribute: ", key]
    decodeAttributeData accessorData = case accessorData of
      (Vec3Float xs) -> Right $ vec3Attribute xs
      (Vec2Float xs) -> Right $ vec2Attribute xs
      _ -> Left $ unwords ["Unsupported attribute accessor data:", show accessorData]
    decodeIndexData accessorData = case accessorData of
      (ScalarShort xs) -> Right $ shortIndex xs
      _ -> Left $ unwords ["Unsupported index accessor data:", show accessorData]
    decodeMode n = case n of
      0 -> Right Points
      1 -> Right Lines
      2 -> Right LineLoop
      3 -> Right LineStrip
      4 -> Right Triangles
      5 -> Right TriangleStrip
      6 -> Right TriangleFan
      _ -> Left $ unwords ["Unkown mode:", show n]

decodeMaterial :: Gltf.Material -> Either String Model.Material
decodeMaterial (Gltf.Material {..}) =
  Model.Material name
    <$> decodePbrUnsafe
      (maybe defaultPbrMetallicRoughness (<> defaultPbrMetallicRoughness) pbrMetallicRoughness)
  where
    decodePbrUnsafe (Gltf.PbrMetallicRoughness {..}) = do
      baseColorFactor <- decodeV4 $ fromJust baseColorFactor
      return
        Model.PbrMetallicRoughness
          { baseColorFactor,
            baseColorTexture = Nothing,
            metallicFactor = fromJust metallicFactor,
            roughnessFactor = fromJust roughnessFactor,
            metallicRoughnessTexture = Nothing
          }

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
