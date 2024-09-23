module Main (main) where

import Core.Encode (encodeScene)
import Core.Model (Scene (..))
import Gltf.Encode (writeGltf)

main :: IO ()
main = do
  writeGltf
    "scene1.gltf"
    ( encodeScene $
        Scene
          { name = pure "Scene",
            nodes = []
          }
    )
