module Gltf.Primitive.Types (module Gltf.Primitive.Types) where

import Control.Monad.Trans.RWS
import Data.ByteString.Lazy
import Data.Map
import Gltf.Json (Accessor, BufferView)

data EncodedPrimitive = EncodedPrimitive
  { attributes :: Map String Int,
    indices :: Maybe Int
  }

data EncodingState = EncodingState
  { accessorIndexOffset :: Int,
    bufferIndex :: Int,
    bufferViewIndex :: Int,
    bufferViewByteOffset :: Int,
    accessorByteOffset :: Int
  }

initialEncoding :: EncodingState
initialEncoding =
  EncodingState
    { accessorIndexOffset = 0,
      bufferIndex = 0,
      bufferViewIndex = 0,
      bufferViewByteOffset = 0,
      accessorByteOffset = 0
    }

type EncodingM = RWS () MeshPart EncodingState

data MeshPart = MeshPart
  { bytes :: [ByteString],
    accessors :: [Accessor],
    bufferViews :: [BufferView]
  }
  deriving (Eq, Show)

instance Semigroup MeshPart where
  (<>)
    (MeshPart {bytes = bytes1, accessors = accessors1, bufferViews = bufferViews1})
    (MeshPart {bytes = bytes2, accessors = accessors2, bufferViews = bufferViews2}) =
      MeshPart
        { bytes = bytes1 <> bytes2,
          accessors = accessors1 <> accessors2,
          bufferViews = bufferViews1 <> bufferViews2
        }

instance Monoid MeshPart where
  mempty =
    MeshPart
      { bytes = [],
        accessors = [],
        bufferViews = []
      }
