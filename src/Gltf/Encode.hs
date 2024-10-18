module Gltf.Encode (module Gltf.Encode) where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty
import Data.Validity (prettyValidate)
import Gltf.Json (Gltf)
import Gltf.Validate (Validity)
import Lib.Base (eitherFail)
import Lib.File (writeNestedFile)

writeGltf :: FilePath -> Gltf -> IO ()
writeGltf filePath = validateBefore $ writeNestedFile filePath . encode

writeGltfPretty :: FilePath -> Gltf -> IO ()
writeGltfPretty filePath = validateBefore $ writeNestedFile filePath . encodePretty

validateBefore :: (MonadFail m, Validity a) => (a -> m b) -> a -> m b
validateBefore action x = eitherFail (prettyValidate x) >>= action
