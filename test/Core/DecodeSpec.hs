{-# LANGUAGE OverloadedStrings #-}

module Core.DecodeSpec (spec) where

import qualified Data.Map as M

import Test.Hspec

import Gltf.Json

spec :: Spec
spec = do
    let gltf = Gltf {
        accessors = gltfList [
                        Accessor {
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
                    Buffer {
                            byteLength = 36,
                            name = Nothing,
                            uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                        }
                    ],
        bufferViews = gltfList [
                            BufferView {
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
                    Mesh {
                        name = Nothing,
                        primitives = [
                                Primitive {
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
                    Node {
                        children = Nothing,
                        matrix = Nothing,
                        mesh = Just 0,
                        name = Nothing
                    }
                ],
        samplers = Nothing,
        scene = Just 0,
        scenes = gltfList [
            Scene {
                name = Nothing,
                nodes = Just [0]
            }],
        textures = Nothing
    }
    describe "Decode triangle without indices" $ do
        it "simething" $ do
            2 + 2 `shouldBe` 4
