module Geometry.Box (module Geometry.Box) where

import Geometry.Plane (tiledPlane)
import Geometry.Types (Geometry, concatGeometries)
import Linear

box :: Float -> Float -> Float -> Geometry
box a b c =
  concatGeometries
    [ tiledPlane (V3 (-(a / 2)) (-(b / 2)) (c / 2)) (V3 a 0 0) (V3 0 b 0) 1 1,
      tiledPlane (V3 (a / 2) (-(b / 2)) (c / 2)) (V3 0 0 (-a)) (V3 0 b 0) 1 1,
      tiledPlane (V3 (a / 2) (-(b / 2)) (-(c / 2))) (V3 (-a) 0 0) (V3 0 b 0) 1 1,
      tiledPlane (V3 (-(a / 2)) (-(b / 2)) (-(c / 2))) (V3 0 0 a) (V3 0 b 0) 1 1,
      tiledPlane (V3 (a / 2) (b / 2) (-(c / 2))) (V3 a 0 0) (V3 0 0 (-c)) 1 1,
      tiledPlane (V3 (a / 2) (-(b / 2)) (-(c / 2))) (V3 (-a) 0 0) (V3 0 b 0) 1 1
    ]
