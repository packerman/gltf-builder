module Gltf.Decode
  ( decodeAccessor,
    DataUrl (..),
    getBufferViewBytes,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Extra (maybeToEither)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Gltf.Accessor (AccessorData)
import Gltf.Decode.Accessor (DecodeOptions (..), decodeAccessorData)
import Gltf.Json
import Lib.Base64 (DataUrl (..))

getBufferViewBytes :: Vector ByteString -> BufferView -> ByteString
getBufferViewBytes
  deliveryBuffers
  (BufferView {buffer, byteOffset, byteLength}) =
    slice (fromMaybe 0 byteOffset) byteLength $ deliveryBuffers V.! buffer
    where
      slice offset sliceLength = BS.take sliceLength . BS.drop offset

decodeOptions :: BufferView -> Accessor -> DecodeOptions
decodeOptions
  (BufferView {byteOffset = bufferViewOffset, byteStride})
  (Accessor {count, accessorType, componentType, byteOffset = accessorOffset}) =
    let byteOffset = fromMaybe 0 bufferViewOffset + fromMaybe 0 accessorOffset
     in DecodeOptions
          { count,
            accessorType,
            componentType,
            byteOffset,
            byteStride
          }

decodeAccessor :: Vector BSL.ByteString -> Vector BufferView -> Accessor -> Either String AccessorData
decodeAccessor
  buffers
  bufferViews
  accessor@(Accessor {bufferView = bufferViewIndex}) = do
    bufferView <- maybeToEither "buffer view index error" (bufferViewIndex >>= (bufferViews !?))
    buffer <- maybeToEither "buffer index error" (buffers !? buffer bufferView)
    let options = decodeOptions bufferView accessor
    decodeAccessorData options buffer
