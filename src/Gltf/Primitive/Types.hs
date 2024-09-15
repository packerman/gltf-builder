module Gltf.Primitive.Types (module Gltf.Primitive.Types) where

import Data.ByteString.Lazy
import Data.Map
import Gltf.Json

data EncodedAccessor = EncodedAccessor
  { accessorBytes :: ByteString,
    accessor :: Accessor,
    accessorIndex :: Int
  }

data EncodedIndices = EncodedIndices
  { accessor :: EncodedAccessor,
    bufferView :: BufferView
  }

data EncodedStrideGroup = EncodedStrideGroup
  { attributes :: Map String EncodedAccessor,
    bufferView :: BufferView
  }
