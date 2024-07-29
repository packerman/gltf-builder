{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Decode (decodeScene) where

import Data.Vector ((!?))

import qualified Gltf.Json as Gltf (Gltf(..))
import qualified Gltf.Json as Scene (Scene(..))
import Gltf.Json (Gltf, GltfList, Buffer, toVector)
import Core.Model
import Data.ByteString (ByteString)

getIndexed :: GltfList a -> Int -> String -> Either String a
getIndexed array index name =
    case array >>= (!? index) of
        Nothing -> Left (name ++ " at index " ++ show index ++ " does not exist")
        Just item -> Right item

data DecodedBuffer = DecodedBuffer {
    bufferData :: ByteString
} deriving (Eq, Show)

decodeBuffer :: Buffer -> DecodedBuffer
decodeBuffer = undefined

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene index gltf = do
    gltfScene <- getIndexed (Gltf.scenes gltf) index "scene"
    let buffers = decodeBuffer <$> toVector (Gltf.buffers gltf)
    return $ scene (Scene.name gltfScene) []
