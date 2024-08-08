module Util.Base64 (
    decodeBase64Text,
    decodeBase64Uri,
    encodeBase64Text
) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Util.Either (mapLeft)

decodeBase64Text :: T.Text -> Either String B.ByteString
decodeBase64Text = mapLeft T.unpack . decodeBase64 . encodeUtf8

base64Prefix :: T.Text
base64Prefix = "data:application/octet-stream;base64,"
base64PrefixLength :: Int
base64PrefixLength = T.length base64Prefix

decodeBase64Uri :: T.Text -> Either String B.ByteString
decodeBase64Uri uri | base64Prefix `T.isPrefixOf` uri =
                        let dataPart = T.drop base64PrefixLength uri
                        in decodeBase64Text dataPart
                    | otherwise = Left "Unsupported uri format"

encodeBase64Text :: B.ByteString -> T.Text
encodeBase64Text = encodeBase64
