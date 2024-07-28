module Core.Decode where

import Data.Vector ((!?))

import qualified Gltf.Json as TF
import Gltf.Json (Gltf, GltfList)
import Core.Model

getIndexed :: GltfList a -> Int -> String -> Either String a
getIndexed array index name =
    case array >>= (!? index) of
        Nothing -> Left (name ++ " at index " ++ show index ++ " does not exist")
        Just item -> Right item

decodeScene :: Int -> Gltf -> Either String Scene
decodeScene index gltf = do
    gltfScene <- getIndexed (TF.scenes gltf) index "scene"
    return scene (TF.name gltfScene) []
