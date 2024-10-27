module Geometry.Box (module Geometry.Box) where

import Geometry.Plane (tiledPlane)
import Geometry.Types (Geometry, concatGeometries)
import Linear

box :: Float -> Float -> Float -> Geometry
box a b c =
  let halfA = a / 2
      halfB = b / 2
      halfC = c / 2
      width = V3 a 0 0
      height = V3 0 b 0
      depth = V3 0 0 (-c)
   in concatGeometries
        [ tiledPlane (V3 (-halfA) (-halfB) halfC) width height 1 1,
          tiledPlane (V3 halfA (-halfB) halfC) depth height 1 1,
          tiledPlane (V3 halfA (-halfB) (-halfC)) (negated width) height 1 1,
          tiledPlane (V3 (-halfA) (-halfB) (-halfC)) (negated depth) height 1 1,
          tiledPlane (V3 (-halfA) halfB halfC) width depth 1 1,
          tiledPlane (V3 (-halfA) (-halfB) (-halfC)) width (negated depth) 1 1
        ]
