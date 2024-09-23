module Gltf.Encode.Types (module Gltf.Encode.Types) where

import Control.Monad.Trans.RWS
import Data.ByteString.Lazy as BSL
import qualified Data.List as L
import Data.Map
import Gltf.Json (Accessor, Buffer (..), BufferView, Material)
import Lib.Base (sumWith)
import Lib.Base64 (bytesDataUrl, encodeDataUrl)

data EncodedPrimitive = EncodedPrimitive
  { attributes :: Map String Int,
    indices :: Maybe Int
  }

data EncodingState = EncodingState
  { accessorIndexOffset :: Int,
    bufferIndex :: Int,
    bufferViewIndex :: Int,
    bufferViewByteOffset :: Int,
    accessorByteOffset :: Int,
    materialIndex :: Int
  }

initialEncoding :: EncodingState
initialEncoding =
  EncodingState
    { accessorIndexOffset = 0,
      bufferIndex = 0,
      bufferViewIndex = 0,
      bufferViewByteOffset = 0,
      accessorByteOffset = 0,
      materialIndex = 0
    }

newBuffer :: Int -> EncodingState -> EncodingState
newBuffer val s = s {bufferIndex = val, bufferViewByteOffset = 0}

setMaterialIndex :: Int -> EncodingState -> EncodingState
setMaterialIndex val s = s {materialIndex = val}

type EncodingM = RWS EncodingOptions MeshPart EncodingState

data MeshPart = MeshPart
  { bytes :: [ByteString],
    buffers :: [Buffer],
    accessors :: [Accessor],
    bufferViews :: [BufferView],
    materials :: [Material]
  }
  deriving (Eq, Show)

fromBufferView :: BufferView -> MeshPart
fromBufferView bufferView =
  mempty
    { bufferViews = L.singleton bufferView
    }

fromAccessor :: Accessor -> ByteString -> MeshPart
fromAccessor accessor bytes =
  mempty
    { accessors = L.singleton accessor,
      bytes = L.singleton bytes
    }

fromMaterial :: Material -> MeshPart
fromMaterial material = mempty {materials = L.singleton material}

withBuffer :: EncodingM a -> EncodingM a
withBuffer action = do
  result <- censor collectBytes action
  markNewBuffer
  return result
  where
    collectBytes :: MeshPart -> MeshPart
    collectBytes meshPart@(MeshPart {bytes}) =
      meshPart
        { buffers = [encodeBuffer bytes],
          bytes = []
        }
      where
        encodeBuffer :: [ByteString] -> Buffer
        encodeBuffer byteStrings =
          Buffer
            { byteLength = fromIntegral $ sumWith BSL.length byteStrings,
              uri = pure $ encodeDataUrl (bytesDataUrl $ BSL.toStrict $ BSL.concat byteStrings),
              name = Nothing
            }
    markNewBuffer :: EncodingM ()
    markNewBuffer = do
      (EncodingState {bufferIndex}) <- get
      modify $ newBuffer (bufferIndex + 1)

instance Semigroup MeshPart where
  (<>)
    ( MeshPart
        { bytes = bytes1,
          buffers = buffers1,
          accessors = accessors1,
          bufferViews = bufferViews1,
          materials = materials1
        }
      )
    ( MeshPart
        { bytes = bytes2,
          buffers = buffers2,
          accessors = accessors2,
          bufferViews = bufferViews2,
          materials = materials2
        }
      ) =
      MeshPart
        { bytes = bytes1 <> bytes2,
          buffers = buffers1 <> buffers2,
          accessors = accessors1 <> accessors2,
          bufferViews = bufferViews1 <> bufferViews2,
          materials = materials1 <> materials2
        }

instance Monoid MeshPart where
  mempty =
    MeshPart
      { bytes = [],
        buffers = [],
        accessors = [],
        bufferViews = [],
        materials = []
      }

newtype EncodingOptions = EncodingOptions
  { bufferCreate :: BufferCreate
  }
  deriving (Eq, Show)

defaultEncodingOptions :: EncodingOptions
defaultEncodingOptions =
  EncodingOptions
    { bufferCreate = SingleBuffer
    }

data BufferCreate = SingleBuffer | OnePerMesh
  deriving (Eq, Show, Enum)
