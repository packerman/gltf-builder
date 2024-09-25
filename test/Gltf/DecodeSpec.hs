module Gltf.DecodeSpec (spec) where

import qualified Data.Map as M
import Gltf.Array
import Gltf.Decode
import Gltf.Json
import Test.Hspec

spec :: Spec
spec = do
  describe "Decode" $ do
    it "reads glTF file" $ do
      let expected =
            ( Gltf
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
                          { byteLength = 648,
                            name = Nothing,
                            uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAAAAAAIA/AAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAAAAAAAAgL8AAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAACAvwAAAAAAAAAAAAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAAAAAAAAAAIC/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAPwAAAL8AAAA/AAAAvwAAAL8AAAA/AAAAPwAAAL8AAAC/AAAAvwAAAL8AAAC/AAAAPwAAAD8AAAA/AAAAPwAAAL8AAAA/AAAAPwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAvwAAAD8AAAA/AAAAPwAAAD8AAAA/AAAAvwAAAD8AAAC/AAAAPwAAAD8AAAC/AAAAvwAAAL8AAAA/AAAAvwAAAD8AAAA/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAvwAAAL8AAAC/AAAAvwAAAD8AAAC/AAAAPwAAAL8AAAC/AAAAPwAAAD8AAAC/AAABAAIAAwACAAEABAAFAAYABwAGAAUACAAJAAoACwAKAAkADAANAA4ADwAOAA0AEAARABIAEwASABEAFAAVABYAFwAWABUA"
                          }
                      ],
                  bufferViews =
                    fromList
                      [ BufferView
                          { buffer = 0,
                            byteOffset = Just 576,
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
                          }
                      ],
                  images = Nothing,
                  materials =
                    fromList
                      [ Material
                          { name = Just "Red",
                            pbrMetallicRoughness =
                              Just
                                ( PbrMetallicRoughness
                                    { baseColorFactor = Just [0.8, 0.0, 0.0, 1.0],
                                      baseColorTexture = Nothing,
                                      metallicFactor = Just 0.0,
                                      roughnessFactor = Nothing,
                                      metallicRoughnessTexture = Nothing
                                    }
                                ),
                            alphaMode = Nothing,
                            alphaCutoff = Nothing,
                            doubleSided = Nothing
                          }
                      ],
                  meshes =
                    fromList
                      [ Mesh
                          { name = Just "Mesh",
                            primitives =
                              [ Primitive
                                  { attributes = M.fromList [("NORMAL", 1), ("POSITION", 2)],
                                    indices = Just 0,
                                    material = Just 0,
                                    mode = Just 4
                                  }
                              ]
                          }
                      ],
                  nodes =
                    fromList
                      [ Node
                          { children = Just [1],
                            matrix = Just [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0],
                            mesh = Nothing,
                            name = Nothing
                          },
                        Node {children = Nothing, matrix = Nothing, mesh = Just 0, name = Nothing}
                      ],
                  samplers = Nothing,
                  scene = Just 0,
                  scenes = fromList [Scene {name = Nothing, nodes = Just [0]}],
                  textures = Nothing
                }
            )
      readGltf "test/data-files/Box.gltf" `shouldReturn` Right expected
