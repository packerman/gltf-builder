module Util (module Util) where

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
