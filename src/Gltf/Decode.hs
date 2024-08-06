{-# LANGUAGE NamedFieldPuns #-}
module Gltf.Decode (
    readGltf,
    decodeBuffer
) where

-- import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Base64 (decodeBase64)
import Gltf.Json
import Util (mapLeft, validate, maybeToEither)
import Gltf.Decode.Accessor (decodeAccessorData, AccessorData)
import Data.Vector (Vector, (!?))

readGltf :: FilePath -> IO (Either String Gltf)
readGltf path = eitherDecode <$> BSL.readFile path

decodeBuffer :: Buffer -> Either String BS.ByteString
decodeBuffer (Buffer { uri = maybeUri, byteLength }) =
    case maybeUri of
        Just uri -> mapLeft T.unpack (decodeUri uri) >>= validateLength byteLength
        Nothing -> error "No uri in buffer"
    where
        base64Prefix = "data:application/octet-stream;base64,"

        decodeUri uri | base64Prefix `T.isPrefixOf` uri =
                let dataPart = T.drop (T.length base64Prefix) uri
                in decodeBase64 $ encodeUtf8 dataPart
            | otherwise = Left "Unsupported uri format"

        validateLength expected value =
            let actual = BS.length value in
                validate (actual == expected)
                        ("Expected buffer length: " ++ show expected ++ ", but actual length is " ++ show actual)
                        value

decodeAccessor :: Vector BSL.ByteString -> Vector BufferView -> Accessor -> Either String AccessorData
decodeAccessor buffers bufferViews (Accessor {
    bufferView = bufferViewIndex,
    count,
    accessorType,
    componentType
}) = do
    bufferView <- maybeToEither "buffer view index error" (bufferViewIndex >>= (bufferViews !?))
    buffer <- maybeToEither "buffer index error" (buffers !? buffer bufferView)
    decodeAccessorData count accessorType componentType buffer
