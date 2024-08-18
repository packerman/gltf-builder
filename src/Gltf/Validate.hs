{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gltf.Validate (Validity (..), Validation) where

import qualified Data.Map as M
import Data.Maybe
import Data.Validity
import qualified Data.Vector as V
import Gltf.Array (Array, toList, (!?))
import Gltf.Json

instance Validity Gltf where
  validate = validateGltf

validateGltf :: Gltf -> Validation
validateGltf
  ( Gltf
      { accessors,
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
    validateAccessors accessors
      <> validateArrayNotEmpty buffers
      <> validateArrayNotEmpty bufferViews
      <> validateArrayNotEmpty images
      <> validateArrayNotEmpty materials
      <> validateMeshes accessors meshes
      <> validateArrayNotEmpty nodes
      <> validateArrayNotEmpty samplers
      <> validateArrayNotEmpty scenes
      <> validateArrayNotEmpty textures
      <> validateDefaultScene scenes scene

validateDefaultScene :: Array Scene -> Maybe Index -> Validation
validateDefaultScene scenes = validateAll (hasIndex scenes)

validateAccessors :: Array Accessor -> Validation
validateAccessors = validateArray (const valid)

validateMeshes :: Array Accessor -> Array Mesh -> Validation
validateMeshes accessors = validateArray validateMesh
  where
    validateMesh (Mesh {primitives}) = decorateList primitives validatePrimitive
    validatePrimitive (Primitive {attributes, indices}) =
      decorateList
        (M.elems attributes)
        ( \index ->
            validateAt accessors index (const valid)
        )
        <> validateAll (hasIndex accessors) indices

validateArray :: (a -> Validation) -> Array a -> Validation
validateArray f array = validateArrayNotEmpty array <> decorateList (toList array) f

validateArrayNotEmpty :: Array a -> Validation
validateArrayNotEmpty array = check (maybe True (not . V.null) array) "The array is not empty."

validateAt :: Array a -> Int -> (a -> Validation) -> Validation
validateAt arr idx func =
  decorate (unwords ["Element at index", show idx]) $
    maybe (invalid "Does not exists.") func (arr !? idx)

hasIndex :: Array a -> Int -> Validation
hasIndex v i = check (isJust $ v >>= (V.!? i)) ("Index " <> show i <> " is not present.")

validateAll :: (Foldable f) => (a -> Validation) -> f a -> Validation
validateAll = foldMap
