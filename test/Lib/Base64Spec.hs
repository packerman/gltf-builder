module Lib.Base64Spec (spec) where

import Data.ByteString as B
import Test.Hspec
import Lib.Base64

spec :: Spec
spec = do
  describe "Base64" $ do
    it "decodes base64 text" $ do
      let input = "AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
          expected = B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0]
      decodeBase64Text input `shouldBe` Right expected
    it "decodes base64 data uri" $ do
      let input = "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
          expected = B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0]
      decodeBase64Url input `shouldBe` Right (DataUrl "application/octet-stream" expected)
    it "decodes media type" $ do
      let input = "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
      let result = decodeBase64Url input
      (isMediaType ["image/png", "application/octet-stream"] <$> result) `shouldBe` Right (Just "application/octet-stream")
      (isMediaType ["image/png"] <$> result) `shouldBe` Right Nothing
    it "encodes base64" $ do
      let input = B.pack [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0]
          expected = "AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
      encodeBase64Text input `shouldBe` expected
