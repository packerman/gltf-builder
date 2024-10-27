module Example (Example (Example), runExample) where

import Core.Encode (writeScene)
import Core.Model

data Example = Example
  { filePath :: FilePath,
    scene :: Scene
  }
  deriving (Eq, Show)

runExample :: Example -> IO ()
runExample (Example {filePath, scene = exampleScene}) = writeScene filePath exampleScene
