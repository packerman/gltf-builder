module Core.ImageEncodeSpec (spec) where

import Core.Dsl as Dsl
import Core.Dsl.Color
import Core.Encode (encodeSceneWithOptions)
import Core.Model as Model
import qualified Data.ByteString as BS
import Data.Default
import Data.Map as M
import qualified Gltf.Array as Array
import Gltf.Delivery (deliveryJson)
import Gltf.Encode.Types (setBufferImages, setSingleBuffer)
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf
import Lib.Base64
import Lib.Image
import Linear
import Test.Hspec

spec :: Spec
spec = describe "Encode images" $ do
  it "encodes images with single buffer" $ do
    let img =
          dataUrlToImage $
            imagePngDataUrl $
              BS.toStrict $
                horizontalGradient red green 8 8
        input =
          Dsl.scene
            [ primitive $
                Primitive
                  { attributes =
                      M.fromList
                        [ ( Position,
                            fromV3List
                              [ V3 (-1) (-1) 0,
                                V3 1 (-1) 0,
                                V3 1 1 0,
                                V3 (-1) 1 0
                              ]
                          ),
                          ( TexCoord 0,
                            fromV2List
                              [ V2 0 0,
                                V2 1 0,
                                V2 1 1,
                                V2 0 1
                              ]
                          ),
                          ( Normal,
                            fromV3List
                              [ V3 0 0 1,
                                V3 0 0 1,
                                V3 0 0 1,
                                V3 0 0 1
                              ]
                          )
                        ],
                    indices = pure $ fromShortList [0, 1, 2, 0, 2, 3],
                    material = Dsl.baseColorTexture $ defaultTextureInfo img,
                    mode = Triangles
                  }
            ]
    (deliveryJson $ encodeSceneWithOptions (def `setBufferImages` True) input)
      `shouldBe` Gltf
        { accessors =
            Array.fromList
              [ Gltf.Accessor {bufferView = Just 0, byteOffset = Just 0, componentType = 5123, count = 6, name = Nothing, accessorType = "SCALAR", max = Just [3.0], min = Just [0.0]},
                Gltf.Accessor {bufferView = Just 1, byteOffset = Just 0, componentType = 5126, count = 4, name = Nothing, accessorType = "VEC2", max = Just [1.0, 1.0], min = Just [0.0, 0.0]},
                Gltf.Accessor {bufferView = Just 2, byteOffset = Just 0, componentType = 5126, count = 4, name = Nothing, accessorType = "VEC3", max = Just [1.0, 1.0, 0.0], min = Just [-1.0, -1.0, 0.0]},
                Gltf.Accessor {bufferView = Just 2, byteOffset = Just 48, componentType = 5126, count = 4, name = Nothing, accessorType = "VEC3", max = Just [0.0, 0.0, 1.0], min = Just [0.0, 0.0, 1.0]}
              ],
          asset = Gltf.Asset {generator = Nothing, version = "2.0"},
          buffers =
            Array.fromList
              [ Gltf.Buffer {byteLength = 243, name = Nothing, uri = Just "data:application/octet-stream;base64,AAABAAIAAAACAAMAAAAAAAAAAAAAAIA/AAAAAAAAgD8AAIA/AAAAAAAAgD8AAIC/AACAvwAAAAAAAIA/AACAvwAAAAAAAIA/AACAPwAAAAAAAIC/AACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD+JUE5HDQoaCgAAAA1JSERSAAAACAAAAAgIAgAAAEttKdwAAAAuSURBVHicdc6BEABACADBg0kmmGSCSSaYXuB+ZgGWA8WGY9LR5ah25Dhi3Sd7PAkfP8GKWR0LAAAAAElFTkSuQmCC"}
              ],
          bufferViews =
            Array.fromList
              [ Gltf.BufferView {buffer = 0, byteOffset = Just 0, byteLength = 12, byteStride = Nothing, name = Nothing, target = Just 34963},
                Gltf.BufferView {buffer = 0, byteOffset = Just 12, byteLength = 32, byteStride = Nothing, name = Nothing, target = Just 34962},
                Gltf.BufferView {buffer = 0, byteOffset = Just 44, byteLength = 96, byteStride = Just 12, name = Nothing, target = Just 34962},
                Gltf.BufferView {buffer = 0, byteOffset = Just 140, byteLength = 103, byteStride = Nothing, name = Nothing, target = Nothing}
              ],
          images =
            Array.fromList
              [ Gltf.Image
                  { name = Nothing,
                    uri = Nothing,
                    mimeType = Just "image/png",
                    bufferView = Just 3
                  }
              ],
          materials =
            Array.fromList
              [ Gltf.Material
                  { name = Nothing,
                    pbrMetallicRoughness =
                      Just
                        ( Gltf.PbrMetallicRoughness
                            { baseColorFactor = Just [1.0, 1.0, 1.0, 1.0],
                              baseColorTexture = Just (Gltf.TextureInfo {index = 0, texCoord = Just 0}),
                              metallicFactor = Just 1.0,
                              roughnessFactor = Just 1.0,
                              metallicRoughnessTexture = Nothing
                            }
                        ),
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
                      [ Gltf.Primitive {attributes = fromList [("NORMAL", 3), ("POSITION", 2), ("TEXCOORD_0", 1)], indices = Just 0, material = Just 0, mode = Just 4}
                      ]
                  }
              ],
          nodes =
            Array.fromList
              [ Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 0, name = Nothing}
              ],
          samplers =
            Array.fromList
              [ Gltf.Sampler {magFilter = Nothing, minFilter = Nothing, name = Nothing, wrapS = Just 10497, wrapT = Just 10497}
              ],
          scene = Just 0,
          scenes = Array.fromList [Gltf.Scene {name = Nothing, nodes = Just [0]}],
          textures = Array.fromList [Gltf.Texture {name = Nothing, sampler = Just 0, source = Just 0}]
        }
  it "encodes images with separate buffers" $ do
    let img =
          dataUrlToImage $
            imagePngDataUrl $
              BS.toStrict $
                horizontalGradient red green 8 8
        input =
          Dsl.scene
            [ primitive $
                Primitive
                  { attributes =
                      M.fromList
                        [ ( Position,
                            fromV3List
                              [ V3 (-1) (-1) 0,
                                V3 1 (-1) 0,
                                V3 1 1 0,
                                V3 (-1) 1 0
                              ]
                          ),
                          ( TexCoord 0,
                            fromV2List
                              [ V2 0 0,
                                V2 1 0,
                                V2 1 1,
                                V2 0 1
                              ]
                          ),
                          ( Normal,
                            fromV3List
                              [ V3 0 0 1,
                                V3 0 0 1,
                                V3 0 0 1,
                                V3 0 0 1
                              ]
                          )
                        ],
                    indices = pure $ fromShortList [0, 1, 2, 0, 2, 3],
                    material = Dsl.baseColorTexture $ defaultTextureInfo img,
                    mode = Triangles
                  }
            ]
    ( deliveryJson $
        encodeSceneWithOptions
          ( def
              `setBufferImages` True
              `setSingleBuffer` False
          )
          input
      )
      `shouldBe` Gltf
        { accessors =
            Array.fromList
              [ Gltf.Accessor {bufferView = Just 0, byteOffset = Just 0, componentType = 5123, count = 6, name = Nothing, accessorType = "SCALAR", max = Just [3.0], min = Just [0.0]},
                Gltf.Accessor {bufferView = Just 1, byteOffset = Just 0, componentType = 5126, count = 4, name = Nothing, accessorType = "VEC2", max = Just [1.0, 1.0], min = Just [0.0, 0.0]},
                Gltf.Accessor {bufferView = Just 2, byteOffset = Just 0, componentType = 5126, count = 4, name = Nothing, accessorType = "VEC3", max = Just [1.0, 1.0, 0.0], min = Just [-1.0, -1.0, 0.0]},
                Gltf.Accessor {bufferView = Just 2, byteOffset = Just 48, componentType = 5126, count = 4, name = Nothing, accessorType = "VEC3", max = Just [0.0, 0.0, 1.0], min = Just [0.0, 0.0, 1.0]}
              ],
          asset = Gltf.Asset {generator = Nothing, version = "2.0"},
          buffers =
            Array.fromList
              [ Gltf.Buffer {byteLength = 140, name = Nothing, uri = Just "data:application/octet-stream;base64,AAABAAIAAAACAAMAAAAAAAAAAAAAAIA/AAAAAAAAgD8AAIA/AAAAAAAAgD8AAIC/AACAvwAAAAAAAIA/AACAvwAAAAAAAIA/AACAPwAAAAAAAIC/AACAPwAAAAAAAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8AAAAAAAAAAAAAgD8="},
                Gltf.Buffer {byteLength = 103, name = Nothing, uri = Just "data:application/octet-stream;base64,iVBORw0KGgoAAAANSUhEUgAAAAgAAAAICAIAAABLbSncAAAALklEQVR4nHXOgRAAQAgAwYNJJphkgkkmmF7gfmYBlgPFhmPS0eWoduQ4Yt0nezwJHz/BilkdCwAAAABJRU5ErkJggg=="}
              ],
          bufferViews =
            Array.fromList
              [ Gltf.BufferView {buffer = 0, byteOffset = Just 0, byteLength = 12, byteStride = Nothing, name = Nothing, target = Just 34963},
                Gltf.BufferView {buffer = 0, byteOffset = Just 12, byteLength = 32, byteStride = Nothing, name = Nothing, target = Just 34962},
                Gltf.BufferView {buffer = 0, byteOffset = Just 44, byteLength = 96, byteStride = Just 12, name = Nothing, target = Just 34962},
                Gltf.BufferView {buffer = 1, byteOffset = Just 0, byteLength = 103, byteStride = Nothing, name = Nothing, target = Nothing}
              ],
          images =
            Array.fromList
              [ Gltf.Image
                  { name = Nothing,
                    uri = Nothing,
                    mimeType = Just "image/png",
                    bufferView = Just 3
                  }
              ],
          materials =
            Array.fromList
              [ Gltf.Material
                  { name = Nothing,
                    pbrMetallicRoughness =
                      Just
                        ( Gltf.PbrMetallicRoughness
                            { baseColorFactor = Just [1.0, 1.0, 1.0, 1.0],
                              baseColorTexture = Just (Gltf.TextureInfo {index = 0, texCoord = Just 0}),
                              metallicFactor = Just 1.0,
                              roughnessFactor = Just 1.0,
                              metallicRoughnessTexture = Nothing
                            }
                        ),
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
                      [ Gltf.Primitive {attributes = fromList [("NORMAL", 3), ("POSITION", 2), ("TEXCOORD_0", 1)], indices = Just 0, material = Just 0, mode = Just 4}
                      ]
                  }
              ],
          nodes =
            Array.fromList
              [ Gltf.Node {children = Nothing, matrix = Nothing, mesh = Just 0, name = Nothing}
              ],
          samplers =
            Array.fromList
              [ Gltf.Sampler {magFilter = Nothing, minFilter = Nothing, name = Nothing, wrapS = Just 10497, wrapT = Just 10497}
              ],
          scene = Just 0,
          scenes = Array.fromList [Gltf.Scene {name = Nothing, nodes = Just [0]}],
          textures = Array.fromList [Gltf.Texture {name = Nothing, sampler = Just 0, source = Just 0}]
        }
