module Core.EncodeSpec (spec) where

import Core.Encode (encodeScene)
import Core.Model as Model
import Data.Map as M
import Gltf.Json (Gltf (..))
import qualified Gltf.Json as Gltf
import Linear
import Test.Hspec

spec :: Spec
spec = do
  describe "Encode" $ do
    it "encodes triangle" $ do
      let input =
            Model.scene
              Nothing
              [ Model.Node
                  { mesh =
                      Just $
                        Model.Mesh
                          Nothing
                          [ Model.Primitive
                              { attributes =
                                  M.fromList
                                    [ ( Position,
                                        fromV3List
                                          [ V3 0 0 0,
                                            V3 1 0 0,
                                            V3 0 1 0
                                          ]
                                      )
                                    ],
                                indices = Nothing,
                                material = Model.defaultMaterial,
                                mode = Triangles
                              }
                          ],
                    children = [],
                    matrix = identity,
                    name = Nothing
                  }
              ]
      encodeScene input
        `shouldBe` Gltf
          { asset =
              Gltf.Asset
                { version = "2.0"
                }
          }
