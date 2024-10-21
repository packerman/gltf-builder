module Geometry.Plane (module Geometry.Plane) where

import Core.Model
import qualified Data.Map as M
import Geometry.Types (Geometry (..), geometry)
import Linear

tiledPlane :: V3 Float -> V3 Float -> V3 Float -> Int -> Int -> Geometry
tiledPlane origin edge1 edge2 m n =
  let normal = normalize $ edge1 `cross` edge2
   in geometry
        ( \material ->
            Primitive
              { attributes =
                  M.fromList
                    [ ( Position,
                        fromV3List $
                          concat
                            [ [position i j, position (i + 1) j, position (i + 1) (j + 1), position i (j + 1)]
                              | i <- [0 .. (m - 1)],
                                j <- [0 .. (n - 1)]
                            ]
                      ),
                      ( TexCoord 0,
                        fromV2List $
                          concat
                            [ [V2 0 0, V2 1 0, V2 1 1, V2 0 1]
                              | _ <- [0 .. (m - 1)],
                                _ <- [0 .. (n - 1)]
                            ]
                      ),
                      ( Normal,
                        fromV3List $
                          concat
                            [ [normal, normal, normal, normal]
                              | _ <- [0 .. (m - 1)],
                                _ <- [0 .. (n - 1)]
                            ]
                      )
                    ],
                indices =
                  pure $
                    fromShortList $
                      concat
                        [ [k, k + 1, k + 2, k, k + 2, k + 3]
                          | i <- [0 .. (m - 1)],
                            j <- [0 .. (n - 1)],
                            let k = fromIntegral $ 4 * (i * m + j)
                        ],
                mode = Triangles,
                material
              }
        )
  where
    position i j =
      let x = fromIntegral i / fromIntegral m
          y = fromIntegral j / fromIntegral n
       in origin ^+^ x *^ edge1 ^+^ y *^ edge2
