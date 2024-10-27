module Lib.Base64
  ( decodeBase64Text,
    decodeBase64Url,
    encodeBase64Text,
    DataUrl (..),
    isMediaType,
    bytesDataUrl,
    imagePngDataUrl,
    encodeDataUrl,
    dataUrl,
  )
where

import qualified Data.ByteString as B
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Either.Extra (mapLeft)
import qualified Data.Text as T
import Data.Text.Encoding (decodeASCII, encodeUtf8)
import Network.Mime (MimeType)

data DataUrl = DataUrl
  { mimeType :: MimeType,
    getData :: B.ByteString
  }
  deriving (Eq, Show)

dataUrl :: MimeType -> B.ByteString -> DataUrl
dataUrl = DataUrl

bytesDataUrl :: B.ByteString -> DataUrl
bytesDataUrl = dataUrl "application/octet-stream"

imagePngDataUrl :: B.ByteString -> DataUrl
imagePngDataUrl = dataUrl "image/png"

decodeBase64Text :: T.Text -> Either String B.ByteString
decodeBase64Text = mapLeft T.unpack . decodeBase64 . encodeUtf8

decodeBase64Url :: T.Text -> Either String DataUrl
decodeBase64Url uri = case T.splitOn "," uri of
  [beforePart, dataPart] -> DataUrl <$> decodeBefore beforePart <*> decodeBase64Text dataPart
  _ -> unsupportedError "only one data part is allowed"
  where
    decodeBefore beforePart = case T.splitOn ":" beforePart of
      ["data", rest] -> decodeMediaType rest
      _ -> unsupportedError "`data` prefix is required"
    decodeMediaType rest = case T.splitOn ";" rest of
      [mediatype, "base64"] -> Right $ encodeUtf8 mediatype
      _ -> unsupportedError "`base64` extension is required"
    unsupportedError = Left . ("Unsupported uri format: " <>)

isMediaType :: [MimeType] -> DataUrl -> Maybe MimeType
isMediaType acceptedMediaType (DataUrl {mimeType}) =
  if mimeType `elem` acceptedMediaType then Just mimeType else Nothing

encodeBase64Text :: B.ByteString -> T.Text
encodeBase64Text = encodeBase64

encodeDataUrl :: DataUrl -> T.Text
encodeDataUrl (DataUrl {mimeType, getData}) =
  "data:" <> decodeASCII mimeType <> ";base64," <> encodeBase64Text getData
