{-# LANGUAGE NamedFieldPuns #-}

module Gltf.Decode
  ( readGltf,
    decodeBuffer,
    BSL.fromStrict,
    decodeAccessor,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Vector (Vector, (!?))
import Gltf.Accessor (AccessorData, decodeAccessorData)
import Gltf.Json
import Util.Base64 (decodeBase64Uri)
import Util.Either (maybeToEither, validate)

readGltf :: FilePath -> IO (Either String Gltf)
readGltf path = eitherDecode <$> BSL.readFile path

decodeBuffer :: Buffer -> Either String BS.ByteString
decodeBuffer (Buffer {uri = maybeUri, byteLength}) =
  case maybeUri of
    Just uri -> decodeBase64Uri uri >>= validateLength byteLength
    Nothing -> error "No uri in buffer"
  where
    validateLength expected value =
      let actual = BS.length value
       in validate
            (actual == expected)
            ("Expected buffer length: " ++ show expected ++ ", but actual length is " ++ show actual)
            value

decodeAccessor :: Vector BSL.ByteString -> Vector BufferView -> Accessor -> Either String AccessorData
decodeAccessor
  buffers
  bufferViews
  ( Accessor
      { bufferView = bufferViewIndex,
        count,
        accessorType,
        componentType
      }
    ) = do
    bufferView <- maybeToEither "buffer view index error" (bufferViewIndex >>= (bufferViews !?))
    buffer <- maybeToEither "buffer index error" (buffers !? buffer bufferView)
    decodeAccessorData count accessorType componentType buffer
