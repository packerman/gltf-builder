module Gltf.ValidateSpec (spec) where

import Data.Default
import Data.Validity
import Gltf.Array (fromList)
import Gltf.Json
import Gltf.Validate ()
import Test.Hspec

spec :: Spec
spec = do
  describe "Validate" $ do
    -- See https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_bytestride
    it "enforces bufferView.byteStride requirements" $ do
      let gltf =
            (def :: Gltf)
              { buffers =
                  fromList
                    [ Buffer
                        { byteLength = 10,
                          name = Nothing,
                          uri = Nothing
                        }
                    ],
                bufferViews =
                  fromList
                    [ BufferView
                        { buffer = 0,
                          byteOffset = pure 0,
                          byteLength = 0,
                          byteStride = Nothing,
                          name = Nothing,
                          target = Nothing
                        }
                    ],
                accessors =
                  fromList
                    [ Accessor
                        { bufferView = pure 0,
                          byteOffset = Nothing,
                          componentType = 5120,
                          count = 10,
                          name = Nothing,
                          accessorType = "VEC3",
                          max = Nothing,
                          min = Nothing
                        },
                      Accessor
                        { bufferView = pure 0,
                          byteOffset = Nothing,
                          componentType = 5120,
                          count = 10,
                          name = Nothing,
                          accessorType = "VEC3",
                          max = Nothing,
                          min = Nothing
                        }
                    ]
              }
      validate gltf `shouldBe` invalid "The following buffer views should have byte stride defined because they are used by more than one accessor: [BufferView {buffer = 0, byteOffset = Just 0, byteLength = 0, byteStride = Nothing, name = Nothing, target = Nothing}]"
