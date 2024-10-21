module Geometry.Types
  ( Geometry (makePrimitive),
    geometry,
  )
where

import Core.Model

newtype Geometry = Geometry
  { makePrimitive :: Material -> Primitive
  }

geometry :: (Material -> Primitive) -> Geometry
geometry = Geometry

concatGeometry :: Geometry -> Geometry -> Maybe Geometry
concatGeometry g1 g2 = undefined
