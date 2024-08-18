module Gltf.Array
  ( Array,
    fromList,
    toVector,
    toList,
    (!),
    (!?),
  )
where

import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V

type Array a = Maybe (Vector a)

fromList :: [a] -> Array a
fromList [] = Nothing
fromList xs = Just $ V.fromList xs

toList :: Array a -> [a]
toList = maybe [] V.toList

toVector :: Array a -> Vector a
toVector = fromMaybe V.empty

(!) :: Array a -> Int -> a
(!) a i = fromJust a V.! i

(!?) :: Array a -> Int -> Maybe a
(!?) a i = a >>= (V.!? i)
