module Data.AttributeSpec (spec) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Test.Hspec

import Linear (V3(..))

import Util.Base64 (decodeBase64Text)
import Data.Attribute

spec :: Spec
spec = do
    describe "Attribute data" $ do
        it "decodes vec3 vector" $ do
            let base64Data = decodeBase64Text "AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                expected = fromV3List [V3 0 0 0, V3 1 0 0, V3 0 1 0]
            (decodeAttributeData 3 "VEC3" 5126 . BL.fromStrict =<< base64Data) `shouldBe` Right expected
        it "encodes attribute data" $ do
            let attributeData = fromV3List [V3 0 0 0, V3 1 0 0, V3 0 1 0]
                expectedBytes = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x80,0x3f,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x80,0x3f,0,0,0,0]
                expectedBase64 = "AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
            BL.unpack (encodeAttributeData attributeData) `shouldBe` expectedBytes
