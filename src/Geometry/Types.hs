module Geometry.Types
  ( Geometry (makePrimitive),
    geometry,
    concatGeometries,
  )
where

import Core.Model
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Lib.Base (allSameBy, guarded)

newtype Geometry = Geometry
  { makePrimitive :: Material -> Maybe Primitive
  }

geometry :: (Material -> Maybe Primitive) -> Geometry
geometry = Geometry

concatGeometries :: [Geometry] -> Geometry
concatGeometries gs =
  geometry
    ( \material -> do
        primitives <- getPrimitives material
        let attributeUnion =
              M.unionsWith
                (\ad -> fromMaybe (error "Attribute data inconsistent") . concatAttributeData ad)
                (attributes <$> primitives)
            firstMode = head $ mode <$> primitives
        pure $
          Primitive
            { attributes = attributeUnion,
              indices = concatIndices primitives,
              material,
              mode = firstMode
            }
    )
  where
    getPrimitives material = mapM (`makePrimitive` material) gs >>= guarded primitivesConsistent
    primitivesConsistent ps =
      allSameBy (M.keysSet . attributes) ps
        && allSameBy (isJust . indices) ps
        && allSameBy mode ps
    concatIndices primitives =
      let offsets = scanl (+) 0 $ primitiveCount <$> primitives
          indicesWithOffset = zipWith moveByOffset offsets <$> mapM indices primitives
       in foldr1 concatIndexData <$> indicesWithOffset
