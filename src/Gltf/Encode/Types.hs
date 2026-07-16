module Gltf.Encode.Types
  ( EncodedPrimitive (..),
    EncodingState (..),
    initialEncoding,
    newBuffer,
    setMaterialIndex,
    EncodingM,
    MeshPart (..),
    meshPartEmpty,
    fromAccessor,
    fromAccessors,
    fromMaterial,
    withBuffer,
    EncodingOptions (EncodingOptions, outputVariant, prettyPrint, interleaved),
    isSingleBuffer,
    isBufferImages,
    setSingleBuffer,
    setBufferImages,
  )
where

import Control.Monad
import Control.Monad.Trans.RWS
import Data.ByteString.Lazy as BSL
import Data.Default
import qualified Data.List as L
import Data.Map
import Gltf.Json (Accessor, Buffer (..), BufferView, Material)
import Lib.Base (sumWith)
import Lib.Base64 (bytesDataUrl, encodeDataUrl)
import Types (GltfVariant (..))

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

meshPartEmpty :: MeshPart -> Bool
meshPartEmpty (MeshPart {..}) =
  L.null bytes
    && L.null buffers
    && L.null accessors
    && L.null bufferViews
    && L.null materials

fromAccessor :: Accessor -> ByteString -> MeshPart
fromAccessor accessor bytes =
  mempty
    { accessors = L.singleton accessor,
      bytes = L.singleton bytes
    }

fromAccessors :: [Accessor] -> ByteString -> MeshPart
fromAccessors accessors bytes =
  mempty
    { accessors,
      bytes = L.singleton bytes
    }

fromMaterial :: Material -> MeshPart
fromMaterial material = mempty {materials = L.singleton material}

withBuffer :: EncodingM a -> EncodingM a
withBuffer action = do
  options <- ask
  (result, meshPart) <- listen $ censor (collectBytes options) action
  unless (meshPartEmpty meshPart) markNewBuffer
  return result
  where
    collectBytes :: EncodingOptions -> MeshPart -> MeshPart
    collectBytes options meshPart@(MeshPart {bytes})
      | meshPartEmpty meshPart = meshPart
      | outputVariant options == GltfBinary =
          meshPart {buffers = [rawBuffer bytes]}
      | otherwise =
          meshPart {buffers = [embeddedBuffer bytes], bytes = []}

    rawBuffer :: [ByteString] -> Buffer
    rawBuffer byteStrings =
      Buffer
        { byteLength = fromIntegral $ sumWith BSL.length byteStrings,
          uri = Nothing,
          name = Nothing
        }

    embeddedBuffer :: [ByteString] -> Buffer
    embeddedBuffer byteStrings =
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

data EncodingOptions = EncodingOptions
  { outputVariant :: GltfVariant,
    singleBuffer :: Bool,
    prettyPrint :: Bool,
    interleaved :: Bool,
    bufferImages :: Bool
  }
  deriving (Eq, Show)

instance Default EncodingOptions where
  def =
    EncodingOptions
      { outputVariant = GltfEmbedded,
        singleBuffer = True,
        prettyPrint = False,
        interleaved = False,
        bufferImages = False
      }

isSingleBuffer :: EncodingOptions -> Bool
isSingleBuffer (EncodingOptions {outputVariant, singleBuffer}) =
  case outputVariant of
    GltfBinary -> True
    _ -> singleBuffer

isBufferImages :: EncodingOptions -> Bool
isBufferImages (EncodingOptions {outputVariant, bufferImages}) =
  case outputVariant of
    GltfBinary -> True
    _ -> bufferImages

setSingleBuffer :: EncodingOptions -> Bool -> EncodingOptions
setSingleBuffer options singleBuffer = options {singleBuffer}

setBufferImages :: EncodingOptions -> Bool -> EncodingOptions
setBufferImages options bufferImages = options {bufferImages}
