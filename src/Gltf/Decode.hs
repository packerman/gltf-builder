module Gltf.Decode (module Gltf.Decode) where

import qualified Data.ByteString.Lazy as BSL

import Gltf.Json

readGltf :: FilePath -> IO (Either String Gltf)
readGltf path = eitherDecode <$> BSL.readFile path
