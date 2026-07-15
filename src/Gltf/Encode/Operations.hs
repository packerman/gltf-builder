module Gltf.Encode.Operations
  ( evalEncoding,
    createBufferViewWithByteLength,
    createBufferViewWithBytes,
  )
where

import Control.Monad.Trans.RWS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Gltf.Encode.Types
import Gltf.Json (BufferView (..), Index)

evalEncoding :: EncodingM a -> EncodingOptions -> (a, MeshPart)
evalEncoding action encodingOptions =
  evalRWS action encodingOptions initialEncoding

createBufferViewWithByteLength :: Maybe Int -> Maybe Int -> Int -> EncodingM Index
createBufferViewWithByteLength byteStride target byteLength = do
  (EncodingState {bufferIndex, bufferViewIndex, bufferViewByteOffset}) <- get
  tell $
    mempty
      { bufferViews =
          [ BufferView
              { buffer = bufferIndex,
                byteOffset = pure bufferViewByteOffset,
                byteLength,
                byteStride,
                name = Nothing,
                target
              }
          ]
      }
  modify $ bufferViewState byteLength
  return bufferViewIndex

createBufferViewWithBytes :: ByteString -> EncodingM Index
createBufferViewWithBytes bytes =
  let byteLength = fromIntegral $ BS.length bytes
   in do
        (EncodingState {bufferIndex, bufferViewIndex, bufferViewByteOffset}) <- get
        tell $
          mempty
            { bytes = [bytes],
              bufferViews =
                [ BufferView
                    { buffer = bufferIndex,
                      byteOffset = pure bufferViewByteOffset,
                      byteLength = byteLength,
                      byteStride = Nothing,
                      name = Nothing,
                      target = Nothing
                    }
                ]
            }
        modify $ bufferViewState byteLength
        return bufferViewIndex

bufferViewState :: Int -> EncodingState -> EncodingState
bufferViewState byteLength encodingState@(EncodingState {bufferViewByteOffset, bufferViewIndex}) =
  encodingState
    { bufferViewIndex = bufferViewIndex + 1,
      bufferViewByteOffset = bufferViewByteOffset + byteLength
    }
