module Lib.MimeType (module Lib.MimeType) where

import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.Mime (MimeType)

mimeTypeToString :: MimeType -> String
mimeTypeToString = unpack . decodeUtf8Lenient
