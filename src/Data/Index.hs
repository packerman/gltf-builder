module Data.Index (
    IndexData
) where

import Data.Vector (Vector)
import Data.Word (Word16)

newtype IndexData = Vec3Attribute (Vector Word16)
    deriving (Eq, Show)
