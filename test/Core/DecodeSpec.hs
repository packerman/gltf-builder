{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Core.DecodeSpec (spec) where

import qualified Data.Map as M

import Test.Hspec

import Gltf.Json as TF
import Core.Decode (decodeScene)
import Core.Model as M

spec :: Spec
spec = do
    let gltf = Gltf {
        accessors = gltfList [
                        TF.Accessor {
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
                    TF.Buffer {
                            byteLength = 36,
                            name = Nothing,
                            uri = Just "data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAACAPwAAAAAAAAAAAAAAAAAAgD8AAAAA"
                        }
                    ],
        bufferViews = gltfList [
                            TF.BufferView {
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
                    TF.Mesh {
                        name = Nothing,
                        primitives = [
                                TF.Primitive {
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
                    TF.Node {
                        children = Nothing,
                        matrix = Nothing,
                        mesh = Just 0,
                        name = Nothing
                    }
                ],
        samplers = Nothing,
        scene = Just 0,
        scenes = gltfList [
            TF.Scene {
                name = Nothing,
                nodes = Just [0]
            }],
        textures = Nothing
    }
    describe "Decode triangle without indices" $ do
        it "something" $ do
            let decoded = decodeScene 0 gltf
            decoded `shouldBe` Right (M.scene Nothing [])
