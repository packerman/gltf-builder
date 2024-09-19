module Lib.UniqList (module Lib.UniqList) where

import Data.Containers.ListUtils
import Data.Map as M (Map, fromList, lookup)

data UniqList a = UniqList
  { elems :: [a],
    index :: Map a Int
  }
  deriving (Eq, Show)

fromList :: (Ord a) => [a] -> UniqList a
fromList xs =
  let elems = nubOrd xs
      index = M.fromList $ zip elems (iterate (+ 1) 0)
   in UniqList {elems, index}

toList :: UniqList a -> [a]
toList (UniqList {elems}) = elems

indexOf :: (Ord a) => a -> UniqList a -> Maybe Int
indexOf x (UniqList {index}) = M.lookup x index
