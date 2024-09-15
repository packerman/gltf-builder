module Base.WriterSpec (spec) where

import Control.Monad.Trans.Writer
import Test.Hspec

spec :: Spec
spec = do
  describe "Writer" $ do
    it "Produces output" $ do
      let w :: Writer [Int] ()
          w = do
            tell [5]
            tell [7]

            return ()
      runWriter w `shouldBe` ((), [5, 7])
