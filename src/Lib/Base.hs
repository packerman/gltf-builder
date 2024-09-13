module Lib.Base (module Lib.Base) where

import Control.Monad.Zip

mcons :: Maybe a -> [a] -> [a]
mcons x xs = maybe xs (: xs) x

sumWith :: (Functor t, Foldable t, Num b) => (a -> b) -> t a -> b
sumWith f = sum . fmap f

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def = maybe (Left def) Right

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right y) = Right y

validateEither :: (a -> Bool) -> b -> Either b a -> Either b a
validateEither _ _ e@(Left _) = e
validateEither p d e@(Right x) = if p x then e else Left d

validate :: Bool -> b -> a -> Either b a
validate p e x = if p then Right x else Left e

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x

doubleToFloat :: Double -> Float
doubleToFloat = realToFrac

pairA :: (Applicative f) => (f a, f b) -> f (a, b)
pairA = uncurry (liftA2 (,))

mzipMin :: (MonadZip m, Ord a, Foldable f) => f (m a) -> m a
mzipMin = foldl1Zip min

mzipMax :: (MonadZip m, Ord a, Foldable f) => f (m a) -> m a
mzipMax = foldl1Zip max

foldl1Zip :: (MonadZip m, Foldable f) => (a -> a -> a) -> f (m a) -> m a
foldl1Zip f = foldl1 (mzipWith f)
