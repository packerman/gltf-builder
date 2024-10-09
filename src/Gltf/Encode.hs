module Gltf.Encode (module Gltf.Encode) where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty
import Gltf.Json (Gltf)
import Lib.File (writeNestedFile)

writeGltf :: FilePath -> Gltf -> IO ()
writeGltf filePath = writeNestedFile filePath . encode

writeGltfPretty :: FilePath -> Gltf -> IO ()
writeGltfPretty filePath = writeNestedFile filePath . encodePretty
