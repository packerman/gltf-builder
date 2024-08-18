{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gltf.Validate (Validity (..), Validation) where

import Data.Validity
import qualified Data.Vector as V
import Gltf.Json

instance Validity Gltf where
  validate = validateGltf

validateGltf :: Gltf -> Validation
validateGltf
  ( Gltf
      { accessors,
        asset,
        buffers,
        bufferViews,
        images,
        materials,
        meshes,
        nodes,
        samplers,
        scene,
        scenes,
        textures
      }
    ) =
    validateGltfArray accessors
      <> validateGltfArray buffers
      <> validateGltfArray bufferViews
      <> validateGltfArray images
      <> validateGltfArray materials
      <> validateGltfArray meshes
      <> validateGltfArray nodes
      <> validateGltfArray samplers
      <> validateGltfArray scenes
      <> validateGltfArray textures

validateGltfArray :: GltfArray a -> Validation
validateGltfArray array = check (maybe True (not . V.null) array) "The array is not empty."
