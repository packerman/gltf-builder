module Main (main) where

import Core.Dsl
import Core.Encode (encodeScene)
import Core.Model
  ( Alpha (..),
    Attribute (..),
    Material (..),
    Mode (..),
    PbrMetallicRoughness (baseColorFactor),
    Primitive (..),
    defaultMaterial,
    defaultPbrMetallicRoughness,
    fromShortList,
    fromV3List,
  )
import qualified Data.Map as M
import Gltf.Encode (writeGltf)
import Linear (V3 (..), V4 (..))

main :: IO ()
main = do
  writeGltf
    "scene1.gltf"
    ( encodeScene $
        scene
          [ primitive $
              Primitive
                { attributes =
                    M.fromList
                      [ ( Position,
                          fromV3List
                            [ V3 (-0.75) (-0.75) 0,
                              V3 0.75 (-0.75) 0,
                              V3 0.75 0.75 0,
                              V3 (-0.75) 0.75 0
                            ]
                        )
                      ],
                  indices = pure $ fromShortList [0, 1, 2, 0, 2, 3],
                  material =
                    defaultMaterial
                      { pbrMetallicRoughness =
                          defaultPbrMetallicRoughness
                            { baseColorFactor = V4 0 0.5 0 1
                            },
                        alpha = Blend,
                        doubleSided = True
                      },
                  mode = Triangles
                }
          ]
    )
