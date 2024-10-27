module Example.TexturedBox (example) where

import Assets
import Core.Dsl as Dsl
import Core.Model hiding (scene)
import Example
import Geometry hiding (geometry)
import System.FilePath

example :: IO Example
example = do
  crate <- dataUrlToImage <$> getCrate
  pure
    $ Example
      ("created-models" </> "textured-box.gltf")
    $ scene
      [ geometry (box 1 1 1) (Dsl.baseColorTexture $ defaultTextureInfo crate)
      ]
