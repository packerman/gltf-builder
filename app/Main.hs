module Main (main) where

import Core.Encode (encodeScene)
import Core.Model (Scene (..))
import Gltf.Encode (writeGltfPretty)

main :: IO ()
main = do
  writeGltfPretty
    "scene1.gltf"
    ( encodeScene $
        Scene
          { name = pure "Scene",
            nodes = []
          }
    )
