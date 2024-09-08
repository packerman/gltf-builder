module Gltf.AccessorSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Gltf.Accessor
import Linear (V3 (..))
import Test.Hspec
import Lib.Base64 (decodeBase64Text)

spec :: Spec
spec = do
  describe "Attribute data" $ do
    it "decodes vec3 vector" $ do
      let base64Data = decodeBase64Text "AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
          expected = fromV3List [V3 0 0 0, V3 1 0 0, V3 0 1 0]
      let options =
            DecodeOptions
              { count = 3,
                accessorType = "VEC3",
                componentType = 5126,
                byteOffset = 0
              }
      (decodeAccessorData options . BL.fromStrict =<< base64Data) `shouldBe` Right expected
    it "encodes attribute data" $ do
      let attributeData = fromV3List [V3 0 0 0, V3 1 0 0, V3 0 1 0]
          expectedBytes = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0]
      BL.unpack (encodeAccessorData attributeData) `shouldBe` expectedBytes
