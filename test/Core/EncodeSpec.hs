module Core.EncodeSpec (spec) where

import Core.Encode (encodeScene)
import Core.Model as Model
import Data.Map as M
import qualified Gltf.Array as Array
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf
import Linear
import Test.Hspec

spec :: Spec
spec = do
  describe "Encode" $ do
    it "encodes triangle" $ do
      let input =
            Model.scene
              Nothing
              [ Model.Node
                  { mesh =
                      Just $
                        Model.Mesh
                          Nothing
                          [ Model.Primitive
                              { attributes =
                                  M.fromList
                                    [ ( Position,
                                        fromV3List
                                          [ V3 0 0 0,
                                            V3 1 0 0,
                                            V3 0 1 0
                                          ]
                                      )
                                    ],
                                indices = Nothing,
                                material = Model.defaultMaterial,
                                mode = Triangles
                              }
                          ],
                    children = [],
                    matrix = identity,
                    name = Nothing
                  }
              ]
      encodeScene input
        `shouldBe` Gltf
          { asset =
              Gltf.Asset
                { version = "2.0",
                  generator = Nothing
                },
            accessors =
              Array.fromList
                [ Gltf.Accessor
                    { bufferView = Just 0,
                      byteOffset = Just 0,
                      componentType = 5126,
                      count = 3,
                      name = Nothing,
                      accessorType = "VEC3",
                      max = Just [1.0, 1.0, 0.0],
                      min = Just [0.0, 0.0, 0.0]
                    }
                ],
            scene = Just 0,
            scenes =
              Array.fromList
                [ Gltf.Scene
                    { name = Nothing,
                      nodes = Just [0]
                    }
                ],
            buffers = Array.fromList [],
            bufferViews =
              Array.fromList
                [ Gltf.BufferView
                    { buffer = 0,
                      byteOffset = Just 0,
                      byteLength = 36,
                      byteStride = Nothing,
                      name = Nothing,
                      target = Just 34962
                    }
                ],
            images = Array.fromList [],
            materials = Array.fromList [],
            meshes =
              Array.fromList
                [ Gltf.Mesh
                    { name = Nothing,
                      primitives =
                        [ Gltf.Primitive
                            { attributes = fromList [("POSITION", 0)],
                              indices = Nothing,
                              material = Nothing,
                              mode = Just 4
                            }
                        ]
                    }
                ],
            nodes = Array.fromList [],
            samplers = Array.fromList [],
            textures = Array.fromList []
          }
