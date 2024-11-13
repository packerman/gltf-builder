module Gltf.Encode.PrimitiveSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Gltf.Accessor (fromV3List)
import Gltf.Encode.Primitive
import Linear
import Test.Hspec

spec :: Spec
spec = do
  describe "Primitive encode" $ do
    it "encodes attribute data" $ do
      let attributeData = fromV3List [V3 0 0 0, V3 1 0 0, V3 0 1 0]
          expectedBytes = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x3f, 0, 0, 0, 0]
      BL.unpack (encodeAccessorData attributeData) `shouldBe` expectedBytes
