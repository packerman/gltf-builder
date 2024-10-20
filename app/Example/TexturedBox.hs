module Example.TexturedBox (example) where

import Assets
import Core.Dsl as Dsl
import Core.Model hiding (scene)
import qualified Data.Map as M
import Example
import Linear
import System.FilePath

example :: IO Example
example = do
  crate <- dataUrlToImage <$> getCrate
  pure
    $ Example
      ("created-models" </> "textured-box.gltf")
    $ scene
      [ primitive $
          Primitive
            { attributes =
                M.fromList
                  [ ( Position,
                      fromV3List
                        [ V3 (-1) (-1) 0,
                          V3 1 (-1) 0,
                          V3 1 1 0,
                          V3 (-1) 1 0
                        ]
                    ),
                    ( TexCoord 0,
                      fromV2List
                        [ V2 0 0,
                          V2 1 0,
                          V2 1 1,
                          V2 0 1
                        ]
                    ),
                    ( Normal,
                      fromV3List
                        [ V3 0 0 1,
                          V3 0 0 1,
                          V3 0 0 1,
                          V3 0 0 1
                        ]
                    )
                  ],
              indices = pure $ fromShortList [0, 1, 2, 0, 2, 3],
              material = Dsl.baseColorTexture $ defaultTextureInfo crate,
              mode = Triangles
            }
      ]
