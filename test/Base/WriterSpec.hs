module Base.WriterSpec (spec) where

import Control.Monad.Trans.Writer
import Data.String (IsString (..))
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
    it "Listens output" $ do
      let inner :: Writer [Int] ()
          inner = do
            tell [10]
            tell [20]
          w :: Writer [Int] [Int]
          w = do
            tell [1, 2]
            (_, out) <- listen inner
            tell [3, 4]
            return out
      runWriter w `shouldBe` ([10, 20], [1, 2, 10, 20, 3, 4])
    it "Transforms output" $ do
      let inner :: Writer [Int] ()
          inner = do
            tell [10]
            tell [20]
          w :: Writer [Int] ()
          w = do
            tell [1, 2]
            censor reverse inner
            tell [3, 4]
            return ()
      runWriter w `shouldBe` ((), [1, 2, 20, 10, 3, 4])
    it "Collects output" $ do
      let w :: Writer TestData ()
          w = do
            tell "a"
            tell "b"
            censor collect $ do
              tell "10"
              tell "20"
            censor collect $ do
              tell "30"
              tell "40"
            return ()
      runWriter w `shouldBe` ((), TestData {elems = "ab", lists = ["1020", "3040"]})

data TestData = TestData
  { elems :: String,
    lists :: [String]
  }
  deriving (Eq, Show)

collect :: TestData -> TestData
collect (TestData {elems}) =
  TestData
    { elems = [],
      lists = [elems]
    }

instance IsString TestData where
  fromString s = TestData {elems = s, lists = []}

instance Semigroup TestData where
  (<>) t1 t2 = TestData {elems = elems t1 <> elems t2, lists = lists t1 <> lists t2}

instance Monoid TestData where
  mempty = TestData {elems = [], lists = []}
