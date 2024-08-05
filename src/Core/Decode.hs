{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Decode (decodeScene, decodeBuffer, byteStringLength) where

import Data.Vector (Vector, (!?))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Base64 (decodeBase64)

import Core.Model as Model
import Gltf.Json (Gltf, GltfArray, Buffer(..), toVector, Index)
import Util (maybeToEither, mapLeft, validate)
import qualified Gltf.Json as Gltf (Gltf(..), Node, Mesh(..))
import qualified Gltf.Json as Scene (Scene(..))
import Linear (V4 (V4), identity)

byteStringLength = BS.length

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene index gltf = do
    gltfScene <- getIndexed (Gltf.scenes gltf) index "scene"
    buffers <- traverse decodeBuffer $ toVector (Gltf.buffers gltf)
    let nodes = decodeNode <$> toVector (Gltf.nodes gltf)
    scene (Scene.name gltfScene) <$> getByIndices nodes "node" (fromMaybe [] $ Scene.nodes gltfScene)

decodeBuffer :: Buffer -> Either String ByteString
decodeBuffer (Buffer { uri = maybeUri, byteLength }) =
    case maybeUri of
        Just uri -> mapLeft T.unpack $ decodeUri uri
        Nothing -> error "No uri in buffer"
    where
        base64prefix = "data:application/octet-stream;base64,"
        decodeUri uri | base64prefix `T.isPrefixOf` uri =
                let dataPart = T.drop (T.length base64prefix) uri
                in decodeBase64 $ encodeUtf8 dataPart
            | otherwise = Left "Unsupported uri format"

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
