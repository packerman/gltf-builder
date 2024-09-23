module Gltf.Encode (module Gltf.Encode) where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL
import Gltf.Json (Gltf)

writeGltf :: FilePath -> Gltf -> IO ()
writeGltf filePath = BSL.writeFile filePath . encode

writeGltfPretty :: FilePath -> Gltf -> IO ()
writeGltfPretty filePath = BSL.writeFile filePath . encodePretty
