module Geometry.Types
  ( Geometry (makePrimitive),
    geometry,
    concatGeometries,
  )
where

import Core.Model

newtype Geometry = Geometry
  { makePrimitive :: Material -> Maybe Primitive
  }

geometry :: (Material -> Maybe Primitive) -> Geometry
geometry = Geometry

concatGeometries :: [Geometry] -> Geometry
concatGeometries gs =
  geometry
    ( \material -> (mapM (`makePrimitive` material) gs) >>= undefined
    -- let ps = mapM (`makePrimitive` material) gs
    --     attr = attributes <$> ps
    --     is = indices <$> ps
    --  in undefined
    )
