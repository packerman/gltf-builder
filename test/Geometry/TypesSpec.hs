module Geometry.TypesSpec (spec) where

import Core.Model
import Data.Default
import qualified Data.Map as M
import Geometry.Types
import Linear
import Test.Hspec

spec :: Spec
spec = do
    describe "Geometry" $ do
        describe "Concatenate" $ do
            let g1 = geometry  (\material -> pure $ Primitive
                                                        { attributes = M.fromList [ (Position, fromV3List [V3 0.1 0.1 0.1]),
                                                                (TexCoord 0, fromV2List [V2 0.11 0.11]),
                                                                (Normal, fromV3List [V3 0.12 0.12 0.12])
                                                            ],
                                                        indices = pure $ fromShortList [0],
                                                        material,
                                                        mode = Triangles
                                                        })
                g2 = geometry  (\material -> pure $ Primitive
                                                            { attributes =
                                                                M.fromList
                                                                    [ (Position, fromV3List [V3 0.2 0.2 0.2]),
                                                                        (TexCoord 0, fromV2List [V2 0.21 0.21]),
                                                                        (Normal, fromV3List [V3 0.22 0.22 0.22])
                                                                    ],
                                                                indices = pure $ fromShortList [0],
                                                                material,
                                                                mode = Triangles
                                                            })
                g3 = geometry  (\material -> pure $ Primitive
                                                            { attributes =
                                                                M.fromList
                                                                    [ (Position, fromV3List [V3 0.3 0.3 0.3]),
                                                                        (TexCoord 0, fromV2List [V2 0.31 0.31]),
                                                                        (Normal, fromV3List [V3 0.32 0.32 0.32])
                                                                    ],
                                                                indices = pure $ fromShortList [],
                                                                material,
                                                                mode = Triangles
                                                            })
                m = def :: Material
            it "concatenates geometries" $ do
                let g = concatGeometries [g1, g2, g3]
                makePrimitive g m `shouldBe` Just  (Primitive {
                                                        attributes = M.fromList
                                                            [ (Position, fromV3List [V3 0.3 0.3 0.3]),
                                                                (TexCoord 0, fromV2List [V2 0.31 0.31]),
                                                                (Normal, fromV3List [V3 0.32 0.32 0.32])
                                                            ],
                                                        indices = pure $ fromShortList [],
                                                        material = m,
                                                        mode = Triangles
                                                         })
