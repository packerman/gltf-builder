{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.DecodeSpec (spec) where

import Core.Decode (decodeScene)
import Core.Model as Model
import qualified Data.Map as M
import Gltf.Json as Gltf
import Linear (V2 (..), V3 (..), V4 (..), identity)
import Test.Hspec

spec :: Spec
spec = do
  describe "Decode" $ do
    it "Decodes triangle without indices" $ do
      let triangleWithoutIndices =
            Gltf
              { accessors =
                  gltfList
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
                asset = defaultAsset,
                buffers =
                  gltfList
                    [ Gltf.Buffer
                        { byteLength = 36,
                          name = Nothing,
                          uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                        }
                    ],
                bufferViews =
                  gltfList
                    [ Gltf.BufferView
                        { buffer = 0,
                          byteOffset = Just 0,
                          byteLength = 36,
                          byteStride = Nothing,
                          name = Nothing,
                          target = Just 34962
                        }
                    ],
                images = Nothing,
                materials = Nothing,
                meshes =
                  gltfList
                    [ Gltf.Mesh
                        { name = Nothing,
                          primitives =
                            [ Gltf.Primitive
                                { attributes =
                                    M.fromList
                                      [ ("POSITION", 0)
                                      ],
                                  indices = Nothing,
                                  material = Nothing,
                                  mode = Nothing
                                }
                            ]
                        }
                    ],
                nodes =
                  gltfList
                    [ Gltf.Node
                        { children = Nothing,
                          matrix = Nothing,
                          mesh = Just 0,
                          name = Nothing
                        }
                    ],
                samplers = Nothing,
                scene = Just 0,
                scenes =
                  gltfList
                    [ Gltf.Scene
                        { name = Nothing,
                          nodes = Just [0]
                        }
                    ],
                textures = Nothing
              }
      let decoded = decodeScene 0 triangleWithoutIndices
      decoded
        `shouldBe` Right
          ( Model.scene
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
          )
    it "Decodes textured box" $ do
      let texturedBox =
            Gltf
              { accessors =
                  gltfList
                    [ Accessor
                        { bufferView = Just 0,
                          byteOffset = Just 0,
                          componentType = 5123,
                          count = 36,
                          name = Nothing,
                          accessorType = "SCALAR",
                          max = Just [23.0],
                          min = Just [0.0]
                        },
                      Accessor
                        { bufferView = Just 1,
                          byteOffset = Just 0,
                          componentType = 5126,
                          count = 24,
                          name = Nothing,
                          accessorType = "VEC3",
                          max = Just [1.0, 1.0, 1.0],
                          min = Just [-1.0, -1.0, -1.0]
                        },
                      Accessor
                        { bufferView = Just 1,
                          byteOffset = Just 288,
                          componentType = 5126,
                          count = 24,
                          name = Nothing,
                          accessorType = "VEC3",
                          max = Just [0.5, 0.5, 0.5],
                          min = Just [-0.5, -0.5, -0.5]
                        },
                      Accessor
                        { bufferView = Just 2,
                          byteOffset = Just 0,
                          componentType = 5126,
                          count = 24,
                          name = Nothing,
                          accessorType = "VEC2",
                          max = Just [6.0, 1.0],
                          min = Just [0.0, 0.0]
                        }
                    ],
                asset =
                  Asset
                    { generator = Just "COLLADA2GLTF",
                      version = "2.0"
                    },
                buffers =
                  gltfList
                    [ Buffer
                        { byteLength = 840,
                          name = Nothing,
                          uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAL8AAAA/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAvwAAAD8AAAC/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAA/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAPwAAAD8AAAC/AADAQAAAAAAAAKBAAAAAAAAAwED+/38/AACgQP7/fz8AAIBAAAAAAAAAoEAAAAAAAACAQAAAgD8AAKBAAACAPwAAAEAAAAAAAACAPwAAAAAAAABAAACAPwAAgD8AAIA/AABAQAAAAAAAAIBAAAAAAAAAQEAAAIA/AACAQAAAgD8AAEBAAAAAAAAAAEAAAAAAAABAQAAAgD8AAABAAACAPwAAAAAAAAAAAAAAAP7/fz8AAIA/AAAAAAAAgD/+/38/AAABAAIAAwACAAEABAAFAAYABwAGAAUACAAJAAoACwAKAAkADAANAA4ADwAOAA0AEAARABIAEwASABEAFAAVABYAFwAWABUA"
                        }
                    ],
                bufferViews =
                  gltfList
                    [ BufferView
                        { buffer = 0,
                          byteOffset = Just 768,
                          byteLength = 72,
                          byteStride = Nothing,
                          name = Nothing,
                          target = Just 34963
                        },
                      BufferView
                        { buffer = 0,
                          byteOffset = Just 0,
                          byteLength = 576,
                          byteStride = Just 12,
                          name = Nothing,
                          target = Just 34962
                        },
                      BufferView
                        { buffer = 0,
                          byteOffset = Just 576,
                          byteLength = 192,
                          byteStride = Just 8,
                          name = Nothing,
                          target = Just 34962
                        }
                    ],
                images =
                  gltfList
                    [ Gltf.Image
                        { name = Nothing,
                          uri = Just "data:image/png;base64,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"
                        }
                    ],
                materials =
                  gltfList
                    [ Gltf.Material
                        { name = Just "Texture",
                          pbrMetallicRoughness =
                            Just
                              ( Gltf.PbrMetallicRoughness
                                  { baseColorFactor = Nothing,
                                    baseColorTexture = Just (TextureInfo {index = 0, texCoord = Nothing}),
                                    metallicFactor = Just 0.0,
                                    roughnessFactor = Nothing,
                                    metallicRoughnessTexture = Nothing
                                  }
                              )
                        }
                    ],
                meshes =
                  gltfList
                    [ Gltf.Mesh
                        { name = Just "Mesh",
                          primitives =
                            [ Gltf.Primitive
                                { attributes = M.fromList [("NORMAL", 1), ("POSITION", 2), ("TEXCOORD_0", 3)],
                                  indices = Just 0,
                                  material = Just 0,
                                  mode = Just 4
                                }
                            ]
                        }
                    ],
                nodes =
                  gltfList
                    [ Gltf.Node
                        { children = Just [1],
                          matrix = Just [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0],
                          mesh = Nothing,
                          name = Nothing
                        },
                      Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 0, name = Nothing}
                    ],
                samplers =
                  gltfList
                    [ Gltf.Sampler
                        { magFilter = Just 9729,
                          minFilter = Just 9986,
                          name = Nothing,
                          wrapS = Just 10497,
                          wrapT = Just 10497
                        }
                    ],
                scene = Just 0,
                scenes =
                  gltfList
                    [ Gltf.Scene
                        { name = Nothing,
                          nodes = Just [0]
                        }
                    ],
                textures =
                  gltfList
                    [ Gltf.Texture
                        { name = Nothing,
                          sampler = Just 0,
                          source = Just 0
                        }
                    ]
              }
      let decoded = decodeScene 0 texturedBox
      decoded
        `shouldBe` Right
          ( Model.Scene
              Nothing
              [ Model.Node
                  { matrix = V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 0.0 (-1.0) 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 0.0 1.0),
                    name = Nothing,
                    mesh = Nothing,
                    children =
                      [ Model.Node
                          { matrix = V4 (V4 1.0 0.0 0.0 0.0) (V4 0.0 1.0 0.0 0.0) (V4 0.0 0.0 1.0 0.0) (V4 0.0 0.0 0.0 1.0),
                            name = Nothing,
                            mesh =
                              Just
                                ( Model.Mesh
                                    { name = Just "Mesh",
                                      primitives =
                                        [ Model.Primitive
                                            { attributes =
                                                M.fromList
                                                  [ (Position, fromV3List [V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5, V3 (-0.5) 0.5 0.5, V3 0.5 0.5 0.5, V3 0.5 0.5 0.5, V3 0.5 (-0.5) 0.5, V3 0.5 0.5 (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) 0.5 0.5, V3 0.5 0.5 0.5, V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 (-0.5), V3 0.5 (-0.5) 0.5, V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 (-0.5) 0.5 0.5, V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) 0.5 (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) 0.5 (-0.5), V3 0.5 (-0.5) (-0.5), V3 0.5 0.5 (-0.5)]),
                                                    (Normal, fromV3List [V3 0.0 0.0 1.0, V3 0.0 0.0 1.0, V3 0.0 0.0 1.0, V3 0.0 0.0 1.0, V3 1.0 0.0 0.0, V3 1.0 0.0 0.0, V3 1.0 0.0 0.0, V3 1.0 0.0 0.0, V3 0.0 1.0 0.0, V3 0.0 1.0 0.0, V3 0.0 1.0 0.0, V3 0.0 1.0 0.0, V3 0.0 (-1.0) 0.0, V3 0.0 (-1.0) 0.0, V3 0.0 (-1.0) 0.0, V3 0.0 (-1.0) 0.0, V3 (-1.0) 0.0 0.0, V3 (-1.0) 0.0 0.0, V3 (-1.0) 0.0 0.0, V3 (-1.0) 0.0 0.0, V3 0.0 0.0 (-1.0), V3 0.0 0.0 (-1.0), V3 0.0 0.0 (-1.0), V3 0.0 0.0 (-1.0)]),
                                                    (TexCoord 0, fromV2List [V2 6.0 0.0, V2 5.0 0.0, V2 6.0 0.9999999, V2 5.0 0.9999999, V2 4.0 0.0, V2 5.0 0.0, V2 4.0 1.0, V2 5.0 1.0, V2 2.0 0.0, V2 1.0 0.0, V2 2.0 1.0, V2 1.0 1.0, V2 3.0 0.0, V2 4.0 0.0, V2 3.0 1.0, V2 4.0 1.0, V2 3.0 0.0, V2 2.0 0.0, V2 3.0 1.0, V2 2.0 1.0, V2 0.0 0.0, V2 0.0 0.9999999, V2 1.0 0.0, V2 1.0 0.9999999])
                                                  ],
                                              indices = Just (fromShortList [0, 1, 2, 3, 2, 1, 4, 5, 6, 7, 6, 5, 8, 9, 10, 11, 10, 9, 12, 13, 14, 15, 14, 13, 16, 17, 18, 19, 18, 17, 20, 21, 22, 23, 22, 21]),
                                              material =
                                                Model.Material
                                                  { name = Just "Texture",
                                                    pbrMetallicRoughness =
                                                      Model.PbrMetallicRoughness
                                                        { baseColorFactor = V4 1.0 1.0 1.0 1.0,
                                                          baseColorTexture = Nothing,
                                                          metallicFactor = 0.0,
                                                          roughnessFactor = 1.0,
                                                          metallicRoughnessTexture = Nothing
                                                        }
                                                  },
                                              mode = Triangles
                                            }
                                        ]
                                    }
                                ),
                            children = []
                          }
                      ]
                  }
              ]
          )
