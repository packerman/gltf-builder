{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Decode (decodeScene) where

import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Data.Maybe
import Linear (V4 (V4), identity, M44)
import qualified Data.ByteString.Lazy as BSL

import Core.Model as Model
import Gltf.Json (Gltf, GltfArray, toVector, Index, Number)
import qualified Gltf.Json as Gltf (Gltf(..), Node (..), Mesh(..), Primitive (..))
import qualified Gltf.Json as Scene (Scene(..))
import Gltf.Decode
import Util.Either (maybeToEither)
import Util.Numeric (doubleToFloat)
import Data.Attribute (AttributeData)

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene index gltf = do
    gltfScene <- getIndexed (Gltf.scenes gltf) index "scene"
    buffers <- traverse (fmap BSL.fromStrict . decodeBuffer) $ toVector $ Gltf.buffers gltf
    let bufferViews = toVector (Gltf.bufferViews gltf)
    accessorData <- traverse (decodeAccessorData buffers bufferViews) $ toVector $ Gltf.accessors gltf
    meshes <- traverse (decodeMesh accessorData) $ toVector $ Gltf.meshes gltf
    nodes <- traverse (decodeNode meshes) $ toVector $ Gltf.nodes gltf
    scene (Scene.name gltfScene) <$> getByIndices nodes "node" (fromMaybe [] $ Scene.nodes gltfScene)

decodeNode :: Vector Model.Mesh -> Gltf.Node -> Either String Node
decodeNode meshes (Gltf.Node { name, matrix = gltfMatrix, mesh = meshIndex }) = do
    matrix <- decodeMatrix gltfMatrix
    mesh <- traverse (getByIndex meshes "mesh") meshIndex
    let children = []
    return Node {..}
    where
        decodeMatrix :: Maybe [Number] -> Either String (M44 Float)
        decodeMatrix = decodeMatrix' . fmap (fmap doubleToFloat)
        decodeMatrix' :: Maybe [Float] -> Either String (M44 Float)
        decodeMatrix' (Just [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]) = Right $
            V4 (V4 a b c d)
                (V4 e f g h)
                (V4 i j k l)
                (V4 m n o p)
        decodeMatrix' Nothing = Right identity
        decodeMatrix' _ = Left "Incorrect matrix: expected 16 numbers"


decodeMesh :: Vector AttributeData -> Gltf.Mesh -> Either String Mesh
decodeMesh attributeData (Gltf.Mesh name primitives) = undefined --Mesh name (decodePrimitive <$> primitives)
    where
        decodePrimitive (Gltf.Primitive {
            attributes,
            indices,
            material,
            mode
            }) = Model.Primitive <$> undefined

        decodeAttribute key = case key of
                                "POSITION" -> Right Position
                                _ -> Left $ "Unknown attribute: " ++ key
        decodeMode n = case n of
                        0 -> Right Points
                        1 -> Right Lines
                        2 -> Right LineLoop
                        3 -> Right LineStrip
                        4 -> Right Triangles
                        5 -> Right TriangleStrip
                        6 -> Right TriangleFan
                        _ -> Left $ "Unkown mode: " ++ show n

getByIndex :: Vector a -> String -> Index -> Either String a
getByIndex v name index = getOrError name index (v !? index)

getByIndices :: Vector a -> String -> [Index] -> Either String [a]
getByIndices v name = traverse (getByIndex v name)

getIndexed :: GltfArray a -> Int -> String -> Either String a
getIndexed array index name = getOrError name index (array >>= (!? index))

getOrError :: String -> Int -> Maybe a -> Either String a
getOrError name index = maybeToEither (indexError name index)
    where
        indexError name index = name ++ " at index " ++ show index ++ " does not exist"
