module Example (Example (Example), runExample) where

import Core.Encode (writeSceneWithOptions)
import Core.Model
import Options

data Example = Example
  { filePath :: FilePath,
    scene :: Scene
  }
  deriving (Eq, Show)

runExample :: Options -> Example -> IO ()
runExample options (Example {filePath, scene = exampleScene}) =
  writeSceneWithOptions (toEncodingOptions options) filePath exampleScene
