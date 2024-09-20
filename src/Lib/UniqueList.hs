module Lib.UniqueList (module Lib.UniqueList) where

import Data.Containers.ListUtils
import Data.Map as M (Map, fromList, lookup)

data UniqueList a = UniqueList
  { elems :: [a],
    index :: Map a Int
  }
  deriving (Eq, Show)

fromList :: (Ord a) => [a] -> UniqueList a
fromList xs =
  let elems = nubOrd xs
      index = M.fromList $ zip elems (iterate (+ 1) 0)
   in UniqueList {elems, index}

toList :: UniqueList a -> [a]
toList (UniqueList {elems}) = elems

indexOf :: (Ord a) => a -> UniqueList a -> Maybe Int
indexOf x (UniqueList {index}) = M.lookup x index
