{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Decode (decodeScene) where

import Data.Vector (Vector, (!?))
import Data.ByteString (ByteString)
import Data.Maybe

import Core.Model as Model
import Gltf.Json (Gltf, GltfArray, Buffer, toVector, Index)
import Util (maybeToEither)
import qualified Gltf.Json as Gltf (Gltf(..), Node, Mesh(..))
import qualified Gltf.Json as Scene (Scene(..))
import Linear (V4 (V4), identity)

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene index gltf = do
    gltfScene <- getIndexed (Gltf.scenes gltf) index "scene"
    let buffers = decodeBuffer <$> toVector (Gltf.buffers gltf)

    let nodes = decodeNode <$> toVector (Gltf.nodes gltf)
    scene (Scene.name gltfScene) <$> getByIndices nodes "node" (fromMaybe [] $ Scene.nodes gltfScene)

decodeBuffer :: Buffer -> ByteString
decodeBuffer = undefined

decodeNode :: Gltf.Node -> Node
decodeNode _ = Model.defaultNode
    where
        decodeMatrix (Just [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]) = Right $
            V4 (V4 a b c d)
                (V4 e f g h)
                (V4 i j k l)
                (V4 m n o p)
        decodeMatrix Nothing = Right identity
        decodeMatrix _ = Left "Incorrect matrix: expected 16 numbers"


decodeMesh :: Gltf.Mesh -> Mesh
decodeMesh (Gltf.Mesh name primitives) = Mesh name (decodePrimitive <$> primitives)
    where
        decodePrimitive = undefined
        decodeMode n = case n of
                        0 -> Right Points
                        1 -> Right Lines
                        2 -> Right LineLoop
                        3 -> Right LineStrip
                        4 -> Right Triangles
                        5 -> Right TriangleStrip
                        6 -> Right TriangleFan
                        _ -> Left $ "Unkown mode: " ++ show n

getByIndices :: Vector a -> String -> [Index] -> Either String [a]
getByIndices v name =
    traverse (\index -> getOrError name index (v !? index))

getIndexed :: GltfArray a -> Int -> String -> Either String a
getIndexed array index name = getOrError name index (array >>= (!? index))

getOrError :: String -> Int -> Maybe a -> Either String a
getOrError name index = maybeToEither (indexError name index)
    where
        indexError name index = name ++ " at index " ++ show index ++ " does not exist"
