module Lib.Container (module Lib.Container) where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Lib.Base

mapPairsM :: (Ord k2, Monad m) => (k1 -> m k2) -> (a -> m b) -> Map k1 a -> m (Map k2 b)
mapPairsM f g m = M.fromList <$> mapM (pairA . bimap f g) (M.toList m)

mapPairs :: (Ord k2) => (k1 -> k2) -> (a -> b) -> Map k1 a -> Map k2 b
mapPairs f g m = M.fromList $ bimap f g <$> M.toList m

groupBy :: (Ord k) => (a -> k) -> [a] -> Map k [a]
groupBy f = M.fromListWith (++) . map (\x -> (f x, [x]))

groupByA :: (Ord k, Applicative f, Monoid (f a)) => (a -> k) -> [a] -> Map k (f a)
groupByA f = M.fromListWith (<>) . fmap (\x -> (f x, pure x))

groupByM :: (Ord k, Monoid m) => (a -> m) -> (a -> k) -> [a] -> Map k m
groupByM s f = M.fromListWith (<>) . fmap (\x -> (f x, s x))

indexList :: (Ord a) => [a] -> Map a Int
indexList = M.fromList . (`zip` iterate (+ 1) 0)

lookupAll :: (Ord k) => [k] -> Map k a -> [a]
lookupAll ks m = mapMaybe (`M.lookup` m) ks
