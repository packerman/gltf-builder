module Gltf.Decode
  ( readGltf,
    decodeBuffer,
    decodeImageData,
    BSL.fromStrict,
    decodeAccessor,
    DataUrl (..),
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Extra (maybeToEither)
import Data.Maybe (fromMaybe)
import Data.Validity (Validity, prettyValidate)
import Data.Vector (Vector, (!?))
import Gltf.Accessor (AccessorData)
import Gltf.Decode.Accessor (DecodeOptions (..), decodeAccessorData)
import Gltf.Json
import Gltf.Validate ()
import Lib.Base (eitherFail, validate)
import Lib.Base64 (DataUrl (..), decodeBase64Url)

readGltf :: FilePath -> IO (Either String Gltf)
readGltf path = validateAfter $ eitherDecode <$> BSL.readFile path

validateAfter :: (MonadFail m, Validity a) => m a -> m a
validateAfter action = action >>= (eitherFail . prettyValidate)

decodeBuffer :: Buffer -> Either String BS.ByteString
decodeBuffer (Buffer {uri = maybeUri, byteLength}) =
  case maybeUri of
    Just uri -> decodeBase64Url uri >>= validateLength byteLength . getData
    Nothing -> error "No uri in buffer"
  where
    validateLength expected value =
      let actual = BS.length value
       in validate
            (actual == expected)
            (unwords ["Expected buffer length:", show expected, "but actual length is", show actual])
            value

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

decodeImageData :: Image -> Either String DataUrl
decodeImageData (Image {uri}) =
  maybeToEither "Image uri is absent" uri >>= decodeBase64Url
