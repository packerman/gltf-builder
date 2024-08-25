module Util.Tuple (module Util.Tuple) where

pairA :: (Applicative f) => (f a, f b) -> f (a, b)
pairA = uncurry (liftA2 (,))
