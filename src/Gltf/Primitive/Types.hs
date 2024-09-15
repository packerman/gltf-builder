module Gltf.Primitive.Types (module Gltf.Primitive.Types) where

import Control.Monad.Trans.State
import Data.ByteString.Lazy
import Data.Map
import Gltf.Json (Accessor, BufferView)

data EncodedPrimitive = EncodedPrimitive
  { attributes :: Map String Int,
    indices :: Maybe Int,
    bytes :: [ByteString],
    accessors :: [Accessor],
    bufferViews :: [BufferView]
  }
  deriving (Eq, Show)

data EncodingState = EncodingState
  { accessorIndexOffset :: Int,
    bufferIndex :: Int,
    bufferViewIndex :: Int,
    bufferViewByteOffset :: Int,
    accessorByteOffset :: Int
  }

type EncodingM = State EncodingState

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
