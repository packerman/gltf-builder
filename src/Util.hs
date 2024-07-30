module Util (module Util) where

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def = maybe (Left def) Right
