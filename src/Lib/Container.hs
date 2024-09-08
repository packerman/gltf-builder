module Lib.Container (module Lib.Container) where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Lib.Base

mapPairsM :: (Ord k2, Monad m) => (k1 -> m k2) -> (a -> m b) -> Map k1 a -> m (Map k2 b)
mapPairsM f g m = M.fromList <$> mapM (pairA . bimap f g) (M.toList m)

mapPairs :: (Ord k2) => (k1 -> k2) -> (a -> b) -> Map k1 a -> Map k2 b
mapPairs f g m = M.fromList $ bimap f g <$> M.toList m

groupBy :: (Ord k) => (a -> k) -> [a] -> Map k [a]
groupBy f = M.fromListWith (++) . map (\x -> (f x, [x]))

groupByA :: (Ord k, Applicative f, Monoid (f a)) => (a -> k) -> [a] -> Map k (f a)
groupByA f = M.fromListWith (<>) . fmap (\x -> (f x, pure x))