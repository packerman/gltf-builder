{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Decode (decodeScene) where

import Data.Vector ((!?))

import qualified Gltf.Json as Gltf (Gltf(..))
import qualified Gltf.Json as Scene (Scene(..))
import Gltf.Json (Gltf, GltfList)
import Core.Model

getIndexed :: GltfList a -> Int -> String -> Either String a
getIndexed array index name =
    case array >>= (!? index) of
        Nothing -> Left (name ++ " at index " ++ show index ++ " does not exist")
        Just item -> Right item

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene index gltf = do
    gltfScene <- getIndexed (Gltf.scenes gltf) index "scene"
    return $ scene (Scene.name gltfScene) []
