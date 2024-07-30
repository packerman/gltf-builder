{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Decode (decodeScene) where

import Data.Vector (Vector, (!?))
import Data.ByteString (ByteString)
import Data.Maybe

import Core.Model as Model
import qualified Gltf.Json as Gltf (Gltf(..), Node(..))
import qualified Gltf.Json as Scene (Scene(..))
import Gltf.Json (Gltf, GltfArray, Buffer, toVector, Index)
import Util (maybeToEither)

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene index gltf = do
    gltfScene <- getIndexed (Gltf.scenes gltf) index "scene"
    let buffers = decodeBuffer <$> toVector (Gltf.buffers gltf)
    let nodes = decodeNode <$> toVector (Gltf.nodes gltf)
    scene (Scene.name gltfScene) <$> getByIndices nodes "node" (fromMaybe [] $ Scene.nodes gltfScene)

getByIndices :: Vector a -> String -> [Index] -> Either String [a]
getByIndices v name =
    traverse (\index -> maybeToEither (name ++ " at index " ++ show index ++ " does not exist") (v !? index))

getIndexed :: GltfArray a -> Int -> String -> Either String a
getIndexed array index name =
    case array >>= (!? index) of
        Nothing -> Left (name ++ " at index " ++ show index ++ " does not exist")
        Just item -> Right item

decodeBuffer :: Buffer -> ByteString
decodeBuffer = undefined

decodeNode :: Gltf.Node -> Node
decodeNode _ = Model.defaultNode
