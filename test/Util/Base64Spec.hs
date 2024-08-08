module Util.Base64Spec (spec) where

import Test.Hspec

import Data.ByteString as B

import Util.Base64

spec :: Spec
spec = do
    describe "Base64" $ do
        it "decodes base64 text" $ do
            let input = "AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                expected = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x80,0x3f,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x80,0x3f,0,0,0,0]
            decodeBase64Text input `shouldBe` Right expected
        it "decodes base64 data uri" $ do
            let input = "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                expected = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x80,0x3f,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x80,0x3f,0,0,0,0]
            decodeBase64Uri input `shouldBe` Right expected
        it "encodes base64" $ do
            let input = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x80,0x3f,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0x80,0x3f,0,0,0,0]
                expected = "AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
            encodeBase64Text input `shouldBe` expected
