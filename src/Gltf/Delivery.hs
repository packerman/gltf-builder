module Gltf.Delivery
  ( Delivery (..),
    jsonToEmbedded,
    readGltfFile,
    readGlbFile,
    writeDelivery,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Extra (maybeToEither)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity (check, prettyValidate)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Gltf.Array (toVector)
import Gltf.Binary.Chunk (getBinaryData, getJsonData, makeGlbFile, writeGlbFile)
import qualified Gltf.Binary.Chunk as Chunk (readGlbFile)
import Gltf.Decode (getBufferViewBytes)
import Gltf.Encode (encodeJson, encodeJsonPretty)
import Gltf.Json
import Gltf.Validate ()
import Lib.Base (eitherFail)
import Lib.Base64 (DataUrl (..), decodeBase64Url)
import Lib.Validity (validationToLeft)
import Types (GltfVariant (..))

data Delivery = Delivery
  { deliveryUri :: Maybe Text,
    deliveryVariant :: GltfVariant,
    deliveryJson :: Gltf,
    deliveryBuffers :: Vector ByteString,
    deliveryImages :: Vector DataUrl
  }
  deriving (Eq, Show)

jsonToEmbedded :: Gltf -> Either String Delivery
jsonToEmbedded = decodeFile Nothing GltfEmbedded Nothing

readGltfFile :: FilePath -> IO Delivery
readGltfFile filePath = do
  jsonData <- BSL.readFile filePath
  eitherFail $ readFileCommon filePath GltfEmbedded jsonData Nothing

readGlbFile :: FilePath -> IO Delivery
readGlbFile filePath = do
  glbFile <- Chunk.readGlbFile filePath
  let jsonData = BSL.fromStrict $ getJsonData glbFile
      binaryData = getBinaryData glbFile
  eitherFail $ readFileCommon filePath GltfBinary jsonData binaryData

readFileCommon :: FilePath -> GltfVariant -> BSL.ByteString -> Maybe ByteString -> Either String Delivery
readFileCommon filePath gltfVariant jsonData binaryData = do
  let uri = Just $ T.pack filePath
  json <- decodeJson jsonData
  decodeFile uri gltfVariant binaryData json

decodeJson :: BSL.ByteString -> Either String Gltf
decodeJson byteString = eitherDecode byteString >>= prettyValidate

decodeFile :: Maybe Text -> GltfVariant -> Maybe ByteString -> Gltf -> Either String Delivery
decodeFile
  deliveryUri
  deliveryVariant
  binaryDataChunk
  deliveryJson@(Gltf {buffers, bufferViews, images}) = do
    deliveryBuffers <- V.iforM (toVector buffers) decodeBuffer
    deliveryImages <- V.forM (toVector images) (decodeImage deliveryBuffers (toVector bufferViews))
    return Delivery {..}
    where
      decodeBuffer :: Int -> Buffer -> Either String ByteString
      decodeBuffer index (Buffer {uri = maybeUri, byteLength}) =
        case maybeUri of
          Just uri' -> decodeBase64Url uri' >>= validateLength byteLength . getData
          Nothing ->
            if index == 0
              then maybeToEither "Binary chunk missing" binaryDataChunk
              else Right BS.empty
        where
          validateLength expected value =
            let actual = BS.length value
             in validationToLeft value $
                  check
                    (actual == expected)
                    (unwords ["Byte buffer length is equal to", show expected])

decodeImage :: Vector ByteString -> Vector BufferView -> Image -> Either String DataUrl
decodeImage bufferBytes bufferViews (Image {uri, mimeType, bufferView}) =
  let decodeImageBuffer = DataUrl (pack $ fromJust mimeType) . getBufferViewBytes bufferBytes
   in case (uri, bufferView) of
        (Just uri', Nothing) -> decodeBase64Url uri'
        (Nothing, Just bufferView') -> Right $ decodeImageBuffer $ bufferViews V.! bufferView'
        _ -> Left "One of uri or bufferView should be specified"

writeDelivery :: FilePath -> Delivery -> IO ()
writeDelivery
  filePath
  ( Delivery
      { deliveryVariant,
        deliveryJson,
        deliveryBuffers
      }
    ) =
    case deliveryVariant of
      GltfBinary ->
        writeGlbFile filePath $
          makeGlbFile
            (BSL.toStrict $ encodeJson deliveryJson)
            (Just $ V.head deliveryBuffers)
      GltfEmbedded -> BSL.writeFile filePath $ encodeJsonPretty deliveryJson
