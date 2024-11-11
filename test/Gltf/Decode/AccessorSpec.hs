module Gltf.Decode.AccessorSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Gltf.Accessor
import Gltf.Decode.Accessor
import Lib.Base64 (decodeBase64Text)
import Linear (V3 (..))
import Test.Hspec

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
                byteOffset = 0,
                byteStride = Nothing
              }
      (decodeAccessorData options . BL.fromStrict =<< base64Data) `shouldBe` Right expected
