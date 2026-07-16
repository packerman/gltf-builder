module Types (module Types) where

data GltfVariant
  = GltfBinary
  | GltfEmbedded
  deriving (Eq, Show, Ord)
