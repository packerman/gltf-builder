module Lib.Base64
  ( decodeBase64Text,
    decodeBase64Url,
    encodeBase64Text,
    DataUrl (..),
    isMediaType,
  )
where

import qualified Data.ByteString as B
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Lib.Base (mapLeft)

type MediaType = T.Text

data DataUrl = DataUrl
  { mediaType :: MediaType,
    getData :: B.ByteString
  }
  deriving (Eq, Show)

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
      [mediatype, "base64"] -> Right mediatype
      _ -> unsupportedError "`base64` extension is required"
    unsupportedError = Left . ("Unsupported uri format: " <>)

isMediaType :: [MediaType] -> DataUrl -> Maybe MediaType
isMediaType acceptedMediaType (DataUrl {mediaType}) =
  if mediaType `elem` acceptedMediaType then Just mediaType else Nothing

encodeBase64Text :: B.ByteString -> T.Text
encodeBase64Text = encodeBase64
