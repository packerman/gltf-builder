{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Core.DecodeSpec (spec) where

import Test.Hspec

import qualified Data.Map as M
import Linear (V3(..), identity)

import Gltf.Json as Gltf
import Core.Decode (decodeScene)
import Core.Model as Model
import Data.Attribute (fromV3List)

spec :: Spec
spec = do
    let gltf = Gltf {
        accessors = gltfList [
                        Gltf.Accessor {
                            bufferView = Just 0,
                            byteOffset = Just 0,
                            componentType = 5126,
                            count = 3,
                            name = Nothing,
                            accessorType = "VEC3",
                            max = Just [1.0,1.0,0.0],
                            min = Just [0.0,0.0,0.0]}
                        ],
        asset = defaultAsset,
        buffers = gltfList [
                    Gltf.Buffer {
                            byteLength = 36,
                            name = Nothing,
                            uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                        }
                    ],
        bufferViews = gltfList [
                            Gltf.BufferView {
                                buffer = 0,
                                byteOffset = Just 0,
                                byteLength = 36,
                                byteStride = Nothing,
                                name = Nothing,
                                target = Just 34962
                            }
                        ],
        images = Nothing,
        materials = Nothing,
        meshes = gltfList [
                    Gltf.Mesh {
                        name = Nothing,
                        primitives = [
                                Gltf.Primitive {
                                    attributes = M.fromList [
                                                    ("POSITION", 0)],
                                    indices = Nothing,
                                    material = Nothing,
                                    mode = Nothing
                                }
                            ]
                        }
                    ],
        nodes = gltfList [
                    Gltf.Node {
                        children = Nothing,
                        matrix = Nothing,
                        mesh = Just 0,
                        name = Nothing
                    }
                ],
        samplers = Nothing,
        scene = Just 0,
        scenes = gltfList [
            Gltf.Scene {
                name = Nothing,
                nodes = Just [0]
            }],
        textures = Nothing
    }
    describe "Decode triangle without indices" $ do
        it "something" $ do
            let decoded = decodeScene 0 gltf
            decoded `shouldBe` Right (Model.scene Nothing [Model.Node {
                mesh = Just $ Model.Mesh Nothing [
                        Model.Primitive {
                            attributes = M.fromList [
                                (Position, fromV3List [
                                        V3 0 0 0, V3 1 0 0, V3 0 1 0
                                    ])
                            ],
                            indices = Nothing,
                            material = Nothing,
                            mode = Triangles
                        }
                    ],
                    children = [],
                    matrix = identity,
                    name = Nothing
                }])
