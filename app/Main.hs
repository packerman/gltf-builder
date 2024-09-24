module Main (main) where

import Core.Encode (encodeScene)
import Core.Model
  ( Attribute (..),
    Material (pbrMetallicRoughness),
    Mesh (..),
    Mode (..),
    Node (..),
    PbrMetallicRoughness (baseColorFactor),
    Primitive (..),
    Scene (..),
    defaultMaterial,
    defaultNode,
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
        Scene
          { name = pure "Scene",
            nodes =
              [ defaultNode
                  { name = pure "Object 1",
                    mesh =
                      pure $
                        Mesh
                          { name = Nothing,
                            primitives =
                              [ Primitive
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
                                              }
                                        },
                                    mode = Triangles
                                  }
                              ]
                          }
                  }
              ]
          }
    )
