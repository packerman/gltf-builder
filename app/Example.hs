module Example (module Example) where

import Core.Model
import Core.Encode (writeScene)

data Example = Example
  { filePath :: FilePath,
    scene :: Scene
  }
  deriving (Eq, Show)

runExample :: Example -> IO ()
runExample (Example {filePath, scene = exampleScene}) = writeScene filePath exampleScene
