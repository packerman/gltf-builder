module Core.EncodeSpec (spec) where

import Core.Encode (encodeScene, encodeSceneWithOptions)
import Core.Model as Model
import qualified Core.Model as Material (Material (..))
import Data.Map as M
import qualified Gltf.Array as Array
import Gltf.Encode.Types
  ( BufferCreate (..),
    EncodingOptions (..),
    defaultEncodingOptions,
  )
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf
import Linear
import Test.Hspec

spec :: Spec
spec = do
  describe "Encode" $ do
    it "encodes empty scene" $ do
      let input = Model.scene (pure "Empty") []
      encodeScene input
        `shouldBe` Gltf
          { asset = Gltf.defaultAsset,
            accessors = Array.empty,
            buffers = Array.empty,
            bufferViews = Array.empty,
            images = Array.empty,
            materials = Array.empty,
            meshes = Array.empty,
            nodes = Array.empty,
            samplers = Array.empty,
            scene = pure 0,
            scenes =
              Array.fromList
                [ Gltf.Scene
                    { name = pure "Empty",
                      nodes = Nothing
                    }
                ],
            textures = Nothing
          }
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
            buffers =
              Array.fromList
                [ Gltf.Buffer
                    { byteLength = 36,
                      name = Nothing,
                      uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                    }
                ],
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
            materials =
              Array.fromList
                [ Gltf.Material
                    { name = Nothing,
                      pbrMetallicRoughness =
                        Just $
                          Gltf.PbrMetallicRoughness
                            { baseColorFactor = Just [1.0, 1.0, 1.0, 1.0],
                              baseColorTexture = Nothing,
                              metallicFactor = Just 1.0,
                              roughnessFactor = Just 1.0,
                              metallicRoughnessTexture = Nothing
                            },
                      alphaMode = Nothing,
                      alphaCutoff = Nothing,
                      doubleSided = Nothing
                    }
                ],
            meshes =
              Array.fromList
                [ Gltf.Mesh
                    { name = Nothing,
                      primitives =
                        [ Gltf.Primitive
                            { attributes = fromList [("POSITION", 0)],
                              indices = Nothing,
                              material = Just 0,
                              mode = Just 4
                            }
                        ]
                    }
                ],
            nodes =
              Array.fromList
                [ Gltf.Node
                    { children = Nothing,
                      matrix = Nothing,
                      mesh = Just 0,
                      name = Nothing
                    }
                ],
            samplers = Array.fromList [],
            textures = Array.fromList []
          }
    describe "encodes many meshes" $ do
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
                                material =
                                  Model.defaultMaterial
                                    { Material.name = pure "Material 1"
                                    },
                                mode = Triangles
                              }
                          ],
                    children = [],
                    matrix = identity,
                    name = Nothing
                  },
                Model.Node
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
                                material =
                                  Model.defaultMaterial
                                    { Material.name = pure "Material 2"
                                    },
                                mode = Triangles
                              }
                          ],
                    children = [],
                    matrix = identity,
                    name = Nothing
                  },
                Model.Node
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
                                                          baseColorTexture = Nothing,
                                                          metallicFactor = 0.0,
                                                          roughnessFactor = 1.0,
                                                          metallicRoughnessTexture = Nothing
                                                        },
                                                    alpha = Opaque,
                                                    doubleSided = False
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
      it "with single buffer" $ do
        encodeScene input
          `shouldBe` Gltf
            { asset =
                Gltf.Asset
                  { version = "2.0",
                    generator = Nothing
                  },
              accessors =
                Array.fromList
                  [ Gltf.Accessor {bufferView = Just 0, byteOffset = Just 0, componentType = 5126, count = 3, name = Nothing, accessorType = "VEC3", max = Just [1.0, 1.0, 0.0], min = Just [0.0, 0.0, 0.0]},
                    Gltf.Accessor {bufferView = Just 1, byteOffset = Just 0, componentType = 5126, count = 3, name = Nothing, accessorType = "VEC3", max = Just [1.0, 1.0, 0.0], min = Just [0.0, 0.0, 0.0]},
                    Gltf.Accessor {bufferView = Just 2, byteOffset = Just 0, componentType = 5123, count = 36, name = Nothing, accessorType = "SCALAR", max = Just [23.0], min = Just [0.0]},
                    Gltf.Accessor {bufferView = Just 3, byteOffset = Just 0, componentType = 5126, count = 24, name = Nothing, accessorType = "VEC2", max = Just [6.0, 1.0], min = Just [0.0, 0.0]},
                    Gltf.Accessor {bufferView = Just 4, byteOffset = Just 0, componentType = 5126, count = 24, name = Nothing, accessorType = "VEC3", max = Just [0.5, 0.5, 0.5], min = Just [-0.5, -0.5, -0.5]},
                    Gltf.Accessor {bufferView = Just 4, byteOffset = Just 288, componentType = 5126, count = 24, name = Nothing, accessorType = "VEC3", max = Just [1.0, 1.0, 1.0], min = Just [-1.0, -1.0, -1.0]}
                  ],
              buffers =
                Array.fromList
                  [ Gltf.Buffer
                      { byteLength = 912,
                        name = Nothing,
                        uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAABAAIAAwACAAEABAAFAAYABwAGAAUACAAJAAoACwAKAAkADAANAA4ADwAOAA0AEAARABIAEwASABEAFAAVABYAFwAWABUAAADAQAAAAAAAAKBAAAAAAAAAwED+/38/AACgQP7/fz8AAIBAAAAAAAAAoEAAAAAAAACAQAAAgD8AAKBAAACAPwAAAEAAAAAAAACAPwAAAAAAAABAAACAPwAAgD8AAIA/AABAQAAAAAAAAIBAAAAAAAAAQEAAAIA/AACAQAAAgD8AAEBAAAAAAAAAAEAAAAAAAABAQAAAgD8AAABAAACAPwAAAAAAAAAAAAAAAP7/fz8AAIA/AAAAAAAAgD/+/38/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAL8AAAA/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAvwAAAD8AAAC/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAA/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAPwAAAD8AAAC/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/"
                      }
                  ],
              bufferViews =
                Array.fromList
                  [ Gltf.BufferView {buffer = 0, byteOffset = Just 0, byteLength = 36, byteStride = Nothing, name = Nothing, target = Just 34962},
                    Gltf.BufferView {buffer = 0, byteOffset = Just 36, byteLength = 36, byteStride = Nothing, name = Nothing, target = Just 34962},
                    Gltf.BufferView {buffer = 0, byteOffset = Just 72, byteLength = 72, byteStride = Nothing, name = Nothing, target = Just 34963},
                    Gltf.BufferView {buffer = 0, byteOffset = Just 144, byteLength = 192, byteStride = Nothing, name = Nothing, target = Just 34962},
                    Gltf.BufferView {buffer = 0, byteOffset = Just 336, byteLength = 576, byteStride = Nothing, name = Nothing, target = Just 34962}
                  ],
              images = Array.fromList [],
              materials =
                Array.fromList
                  [ Gltf.Material
                      { name = Just "Material 1",
                        pbrMetallicRoughness = Just (Gltf.PbrMetallicRoughness {baseColorFactor = Just [1.0, 1.0, 1.0, 1.0], baseColorTexture = Nothing, metallicFactor = Just 1.0, roughnessFactor = Just 1.0, metallicRoughnessTexture = Nothing}),
                        alphaMode = Nothing,
                        alphaCutoff = Nothing,
                        doubleSided = Nothing
                      },
                    Gltf.Material
                      { name = Just "Material 2",
                        pbrMetallicRoughness = Just (Gltf.PbrMetallicRoughness {baseColorFactor = Just [1.0, 1.0, 1.0, 1.0], baseColorTexture = Nothing, metallicFactor = Just 1.0, roughnessFactor = Just 1.0, metallicRoughnessTexture = Nothing}),
                        alphaMode = Nothing,
                        alphaCutoff = Nothing,
                        doubleSided = Nothing
                      },
                    Gltf.Material
                      { name = Just "Texture",
                        pbrMetallicRoughness = Just (Gltf.PbrMetallicRoughness {baseColorFactor = Just [1.0, 1.0, 1.0, 1.0], baseColorTexture = Nothing, metallicFactor = Just 0.0, roughnessFactor = Just 1.0, metallicRoughnessTexture = Nothing}),
                        alphaMode = Nothing,
                        alphaCutoff = Nothing,
                        doubleSided = Nothing
                      }
                  ],
              meshes =
                Array.fromList
                  [ Gltf.Mesh {name = Nothing, primitives = [Gltf.Primitive {attributes = fromList [("POSITION", 0)], indices = Nothing, material = Just 0, mode = Just 4}]},
                    Gltf.Mesh {name = Nothing, primitives = [Gltf.Primitive {attributes = fromList [("POSITION", 1)], indices = Nothing, material = Just 1, mode = Just 4}]},
                    Gltf.Mesh {name = Just "Mesh", primitives = [Gltf.Primitive {attributes = fromList [("NORMAL", 5), ("POSITION", 4), ("TEXCOORD_0", 3)], indices = Just 2, material = Just 2, mode = Just 4}]}
                  ],
              nodes =
                Array.fromList
                  [ Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 0, name = Nothing},
                    Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 1, name = Nothing},
                    Gltf.Node {children = Just [3], matrix = Just [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0], mesh = Nothing, name = Nothing},
                    Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 2, name = Nothing}
                  ],
              samplers = Array.fromList [],
              scene = Just 0,
              scenes = Array.fromList [Gltf.Scene {name = Nothing, nodes = Just [0, 1, 2]}],
              textures = Array.fromList []
            }
      it "with many buffers" $ do
        encodeSceneWithOptions
          ( defaultEncodingOptions
              { bufferCreate = OnePerMesh
              }
          )
          input
          `shouldBe` Gltf
            { asset =
                Gltf.Asset
                  { version = "2.0",
                    generator = Nothing
                  },
              accessors =
                Array.fromList
                  [ Gltf.Accessor {bufferView = Just 0, byteOffset = Just 0, componentType = 5126, count = 3, name = Nothing, accessorType = "VEC3", max = Just [1.0, 1.0, 0.0], min = Just [0.0, 0.0, 0.0]},
                    Gltf.Accessor {bufferView = Just 1, byteOffset = Just 0, componentType = 5126, count = 3, name = Nothing, accessorType = "VEC3", max = Just [1.0, 1.0, 0.0], min = Just [0.0, 0.0, 0.0]},
                    Gltf.Accessor {bufferView = Just 2, byteOffset = Just 0, componentType = 5123, count = 36, name = Nothing, accessorType = "SCALAR", max = Just [23.0], min = Just [0.0]},
                    Gltf.Accessor {bufferView = Just 3, byteOffset = Just 0, componentType = 5126, count = 24, name = Nothing, accessorType = "VEC2", max = Just [6.0, 1.0], min = Just [0.0, 0.0]},
                    Gltf.Accessor {bufferView = Just 4, byteOffset = Just 0, componentType = 5126, count = 24, name = Nothing, accessorType = "VEC3", max = Just [0.5, 0.5, 0.5], min = Just [-0.5, -0.5, -0.5]},
                    Gltf.Accessor {bufferView = Just 4, byteOffset = Just 288, componentType = 5126, count = 24, name = Nothing, accessorType = "VEC3", max = Just [1.0, 1.0, 1.0], min = Just [-1.0, -1.0, -1.0]}
                  ],
              buffers =
                Array.fromList
                  [ Gltf.Buffer
                      { byteLength = 36,
                        name = Nothing,
                        uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                      },
                    Gltf.Buffer
                      { byteLength = 36,
                        name = Nothing,
                        uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                      },
                    Gltf.Buffer
                      { byteLength = 840,
                        name = Nothing,
                        uri = Just "data:application/octet-stream;base64,AAABAAIAAwACAAEABAAFAAYABwAGAAUACAAJAAoACwAKAAkADAANAA4ADwAOAA0AEAARABIAEwASABEAFAAVABYAFwAWABUAAADAQAAAAAAAAKBAAAAAAAAAwED+/38/AACgQP7/fz8AAIBAAAAAAAAAoEAAAAAAAACAQAAAgD8AAKBAAACAPwAAAEAAAAAAAACAPwAAAAAAAABAAACAPwAAgD8AAIA/AABAQAAAAAAAAIBAAAAAAAAAQEAAAIA/AACAQAAAgD8AAEBAAAAAAAAAAEAAAAAAAABAQAAAgD8AAABAAACAPwAAAAAAAAAAAAAAAP7/fz8AAIA/AAAAAAAAgD/+/38/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAL8AAAA/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAvwAAAD8AAAC/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAA/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAPwAAAD8AAAC/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/"
                      }
                  ],
              bufferViews =
                Array.fromList
                  [ Gltf.BufferView {buffer = 0, byteOffset = Just 0, byteLength = 36, byteStride = Nothing, name = Nothing, target = Just 34962},
                    Gltf.BufferView {buffer = 1, byteOffset = Just 0, byteLength = 36, byteStride = Nothing, name = Nothing, target = Just 34962},
                    Gltf.BufferView {buffer = 2, byteOffset = Just 0, byteLength = 72, byteStride = Nothing, name = Nothing, target = Just 34963},
                    Gltf.BufferView {buffer = 2, byteOffset = Just 72, byteLength = 192, byteStride = Nothing, name = Nothing, target = Just 34962},
                    Gltf.BufferView {buffer = 2, byteOffset = Just 264, byteLength = 576, byteStride = Nothing, name = Nothing, target = Just 34962}
                  ],
              images = Array.fromList [],
              materials =
                Array.fromList
                  [ Gltf.Material
                      { name = Just "Material 1",
                        pbrMetallicRoughness = Just (Gltf.PbrMetallicRoughness {baseColorFactor = Just [1.0, 1.0, 1.0, 1.0], baseColorTexture = Nothing, metallicFactor = Just 1.0, roughnessFactor = Just 1.0, metallicRoughnessTexture = Nothing}),
                        alphaMode = Nothing,
                        alphaCutoff = Nothing,
                        doubleSided = Nothing
                      },
                    Gltf.Material
                      { name = Just "Material 2",
                        pbrMetallicRoughness = Just (Gltf.PbrMetallicRoughness {baseColorFactor = Just [1.0, 1.0, 1.0, 1.0], baseColorTexture = Nothing, metallicFactor = Just 1.0, roughnessFactor = Just 1.0, metallicRoughnessTexture = Nothing}),
                        alphaMode = Nothing,
                        alphaCutoff = Nothing,
                        doubleSided = Nothing
                      },
                    Gltf.Material
                      { name = Just "Texture",
                        pbrMetallicRoughness = Just (Gltf.PbrMetallicRoughness {baseColorFactor = Just [1.0, 1.0, 1.0, 1.0], baseColorTexture = Nothing, metallicFactor = Just 0.0, roughnessFactor = Just 1.0, metallicRoughnessTexture = Nothing}),
                        alphaMode = Nothing,
                        alphaCutoff = Nothing,
                        doubleSided = Nothing
                      }
                  ],
              meshes =
                Array.fromList
                  [ Gltf.Mesh {name = Nothing, primitives = [Gltf.Primitive {attributes = fromList [("POSITION", 0)], indices = Nothing, material = Just 0, mode = Just 4}]},
                    Gltf.Mesh {name = Nothing, primitives = [Gltf.Primitive {attributes = fromList [("POSITION", 1)], indices = Nothing, material = Just 1, mode = Just 4}]},
                    Gltf.Mesh {name = Just "Mesh", primitives = [Gltf.Primitive {attributes = fromList [("NORMAL", 5), ("POSITION", 4), ("TEXCOORD_0", 3)], indices = Just 2, material = Just 2, mode = Just 4}]}
                  ],
              nodes =
                Array.fromList
                  [ Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 0, name = Nothing},
                    Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 1, name = Nothing},
                    Gltf.Node {children = Just [3], matrix = Just [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0], mesh = Nothing, name = Nothing},
                    Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 2, name = Nothing}
                  ],
              samplers = Array.fromList [],
              scene = Just 0,
              scenes = Array.fromList [Gltf.Scene {name = Nothing, nodes = Just [0, 1, 2]}],
              textures = Array.fromList []
            }
