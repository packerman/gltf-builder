module Util.Map (module Util.Map) where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Util.Tuple

mapPairsM :: (Ord k2, Monad m) => (k1 -> m k2) -> (a -> m b) -> Map k1 a -> m (Map k2 b)
mapPairsM f g m = M.fromList <$> mapM (pairA . bimap f g) (M.toList m)

mapPairs :: (Ord k2) => (k1 -> k2) -> (a -> b) -> Map k1 a -> Map k2 b
mapPairs f g m = M.fromList $ bimap f g <$> M.toList m
