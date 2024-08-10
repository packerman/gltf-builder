module Util.Map (module Util.Map) where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M

mapPairsM :: (Ord k2, Monad m) => (k1 -> m k2) -> (a -> m b) -> Map k1 a -> m (Map k2 b)
mapPairsM f g m = M.fromList <$> mapM (uncurry (liftA2 (,)) . bimap f g) (M.toList m)
