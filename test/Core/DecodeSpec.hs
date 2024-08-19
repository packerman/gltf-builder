module Core.DecodeSpec (spec) where

import Core.Decode (decodeScene)
import Core.Model as Model
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Gltf.Array as Array
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
                  fromList
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
                  fromList
                    [ Gltf.Buffer
                        { byteLength = 36,
                          name = Nothing,
                          uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                        }
                    ],
                bufferViews =
                  fromList
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
                  fromList
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
                  fromList
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
                  fromList
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
                  fromList
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
                  fromList
                    [ Buffer
                        { byteLength = 840,
                          name = Nothing,
                          uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAL8AAAA/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAvwAAAD8AAAC/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAA/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAPwAAAD8AAAC/AADAQAAAAAAAAKBAAAAAAAAAwED+/38/AACgQP7/fz8AAIBAAAAAAAAAoEAAAAAAAACAQAAAgD8AAKBAAACAPwAAAEAAAAAAAACAPwAAAAAAAABAAACAPwAAgD8AAIA/AABAQAAAAAAAAIBAAAAAAAAAQEAAAIA/AACAQAAAgD8AAEBAAAAAAAAAAEAAAAAAAABAQAAAgD8AAABAAACAPwAAAAAAAAAAAAAAAP7/fz8AAIA/AAAAAAAAgD/+/38/AAABAAIAAwACAAEABAAFAAYABwAGAAUACAAJAAoACwAKAAkADAANAA4ADwAOAA0AEAARABIAEwASABEAFAAVABYAFwAWABUA"
                        }
                    ],
                bufferViews =
                  fromList
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
                  fromList
                    [ Gltf.Image
                        { name = Nothing,
                          uri = Just "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAAEklEQVR4nGP4z8CAB+GTG8HSALfKY52fTcuYAAAAAElFTkSuQmCC"
                        }
                    ],
                materials =
                  fromList
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
                  fromList
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
                  fromList
                    [ Gltf.Node
                        { children = Just [1],
                          matrix = Just [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0],
                          mesh = Nothing,
                          name = Nothing
                        },
                      Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 0, name = Nothing}
                    ],
                samplers =
                  fromList
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
                  fromList
                    [ Gltf.Scene
                        { name = Nothing,
                          nodes = Just [0]
                        }
                    ],
                textures =
                  fromList
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
                                                  [ ( Position,
                                                      fromV3List [V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) 0.5, V3 (-0.5) 0.5 0.5, V3 0.5 0.5 0.5, V3 0.5 0.5 0.5, V3 0.5 (-0.5) 0.5, V3 0.5 0.5 (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) 0.5 0.5, V3 0.5 0.5 0.5, V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 (-0.5), V3 0.5 (-0.5) 0.5, V3 (-0.5) (-0.5) 0.5, V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) (-0.5) 0.5, V3 (-0.5) 0.5 0.5, V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) 0.5 (-0.5), V3 (-0.5) (-0.5) (-0.5), V3 (-0.5) 0.5 (-0.5), V3 0.5 (-0.5) (-0.5), V3 0.5 0.5 (-0.5)]
                                                    ),
                                                    ( Normal,
                                                      fromV3List [V3 0.0 0.0 1.0, V3 0.0 0.0 1.0, V3 0.0 0.0 1.0, V3 0.0 0.0 1.0, V3 1.0 0.0 0.0, V3 1.0 0.0 0.0, V3 1.0 0.0 0.0, V3 1.0 0.0 0.0, V3 0.0 1.0 0.0, V3 0.0 1.0 0.0, V3 0.0 1.0 0.0, V3 0.0 1.0 0.0, V3 0.0 (-1.0) 0.0, V3 0.0 (-1.0) 0.0, V3 0.0 (-1.0) 0.0, V3 0.0 (-1.0) 0.0, V3 (-1.0) 0.0 0.0, V3 (-1.0) 0.0 0.0, V3 (-1.0) 0.0 0.0, V3 (-1.0) 0.0 0.0, V3 0.0 0.0 (-1.0), V3 0.0 0.0 (-1.0), V3 0.0 0.0 (-1.0), V3 0.0 0.0 (-1.0)]
                                                    ),
                                                    ( TexCoord 0,
                                                      fromV2List [V2 6.0 0.0, V2 5.0 0.0, V2 6.0 0.9999999, V2 5.0 0.9999999, V2 4.0 0.0, V2 5.0 0.0, V2 4.0 1.0, V2 5.0 1.0, V2 2.0 0.0, V2 1.0 0.0, V2 2.0 1.0, V2 1.0 1.0, V2 3.0 0.0, V2 4.0 0.0, V2 3.0 1.0, V2 4.0 1.0, V2 3.0 0.0, V2 2.0 0.0, V2 3.0 1.0, V2 2.0 1.0, V2 0.0 0.0, V2 0.0 0.9999999, V2 1.0 0.0, V2 1.0 0.9999999]
                                                    )
                                                  ],
                                              indices =
                                                Just (fromShortList [0, 1, 2, 3, 2, 1, 4, 5, 6, 7, 6, 5, 8, 9, 10, 11, 10, 9, 12, 13, 14, 15, 14, 13, 16, 17, 18, 19, 18, 17, 20, 21, 22, 23, 22, 21]),
                                              material =
                                                Model.Material
                                                  { name = Just "Texture",
                                                    pbrMetallicRoughness =
                                                      Model.PbrMetallicRoughness
                                                        { baseColorFactor = V4 1.0 1.0 1.0 1.0,
                                                          baseColorTexture =
                                                            Just $
                                                              Model.Texture
                                                                { name = Nothing,
                                                                  image =
                                                                    Model.Image
                                                                      { name = Nothing,
                                                                        mimeType = "image/png",
                                                                        imageData = BS.pack [137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 10, 0, 0, 0, 10, 8, 2, 0, 0, 0, 2, 80, 88, 234, 0, 0, 0, 18, 73, 68, 65, 84, 120, 156, 99, 248, 207, 192, 128, 7, 225, 147, 27, 193, 210, 0, 183, 202, 99, 157, 159, 77, 203, 152, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130]
                                                                      },
                                                                  sampler =
                                                                    Model.Sampler
                                                                      { name = Nothing,
                                                                        magFilter = Just MagLinear,
                                                                        minFilter = Just NearestMipmapLinear,
                                                                        wrapS = Repeat,
                                                                        wrapT = Repeat
                                                                      }
                                                                },
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
