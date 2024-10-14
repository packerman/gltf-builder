module Example.Square (example) where

import Core.Dsl
import Core.Model
  ( Attribute (..),
    Mode (..),
    Primitive (..),
    fromShortList,
    fromV3List,
  )
import qualified Data.Map as M
import Example (Example (Example))
import Linear (V3 (..), V4 (..))
import System.FilePath

example :: Example
example =
  Example ("created-models" </> "square.gltf") $
    scene
      [ primitive $
          Primitive
            { attributes =
                M.fromList
                  [ ( Position,
                      fromV3List
                        [ V3 (-0.75) (-0.75) 0,
                          V3 0.75 (-0.75) 0,
                          V3 0.75 0.75 0,
                          V3 (-0.75) 0.75 0
                        ]
                    )
                  ],
              indices = pure $ fromShortList [0, 1, 2, 0, 2, 3],
              material = baseColor $ V4 0 0.5 0 1,
              mode = Triangles
            }
      ]
