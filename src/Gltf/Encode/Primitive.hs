module Gltf.Encode.Primitive
  ( encodePrimitive,
    EncodingM,
    EncodedPrimitive,
    MeshPart,
    encodeAccessorData,
  )
where

import Control.Monad
import Control.Monad.Trans.RWS
import Control.Monad.Zip (MonadZip)
import Data.Binary.Put (Put, putFloatle, putWord16le, runPut)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.List (singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Gltf.Accessor (AccessorData (..))
import Gltf.Encode.Types
import Gltf.Json (Accessor (..), BufferView (..))
import Lib.Base (isSingleton, mzipMax, mzipMin, sumWith)
import Lib.Container (groupBy)
import Linear (V2 (..), V3 (..))
import Numeric.Extra (floatToDouble)

encodePrimitive ::
  Map String AccessorData ->
  Maybe AccessorData ->
  EncodingM EncodedPrimitive
encodePrimitive attributes indices = do
  encodedIndices <- traverse encodeIndices indices
  encodedAttributes <- encodeAttributes attributes
  return
    EncodedPrimitive
      { attributes = M.unions encodedAttributes,
        indices = encodedIndices
      }

encodeAttributes :: Map String AccessorData -> EncodingM [Map String Int]
encodeAttributes attributes =
  let strideGroups = groupBy (stride . snd) (M.assocs attributes)
   in forM (M.assocs strideGroups) (uncurry encodeWithStride)
  where
    encodeWithStride :: Int -> [(String, AccessorData)] -> EncodingM (Map String Int)
    encodeWithStride groupStride attributeList = do
      resetAccessorByteOffset
      let byteSizeSum = sumWith (byteSize . snd) attributeList
      attrs <- forM attributeList (mapM encodeAccessor)
      let bufferViewStride = if isSingleton attributeList then Nothing else Just groupStride
      createBufferView bufferViewStride (pure 34962) byteSizeSum
      return $ M.fromList attrs

encodeIndices :: AccessorData -> EncodingM Int
encodeIndices accessorData =
  do
    resetAccessorByteOffset
    accessor <- encodeAccessor accessorData
    createBufferView Nothing (pure 34963) (byteSize accessorData)
    return accessor

createBufferView :: Maybe Int -> Maybe Int -> Int -> EncodingM ()
createBufferView byteStride target byteLength = do
  (EncodingState {bufferIndex, bufferViewByteOffset}) <- get
  modify bufferViewState
  tell $
    fromBufferView $
      BufferView
        { buffer = bufferIndex,
          byteOffset = pure bufferViewByteOffset,
          byteLength,
          byteStride,
          name = Nothing,
          target
        }
  where
    bufferViewState st@(EncodingState {bufferViewByteOffset, bufferViewIndex}) =
      st
        { bufferViewIndex = bufferViewIndex + 1,
          bufferViewByteOffset = bufferViewByteOffset + byteLength
        }

resetAccessorByteOffset :: EncodingM ()
resetAccessorByteOffset = modify (\s -> s {accessorByteOffset = 0})

encodeAccessor :: AccessorData -> EncodingM Int
encodeAccessor
  accessorData =
    let (accessorType, componentType) = getAccessorTypes
        bytes = encodeAccessorData accessorData
     in do
          ( EncodingState
              { bufferViewIndex,
                accessorByteOffset,
                accessorIndexOffset
              }
            ) <-
            get
          modify $ accessorState $ BSL.length bytes
          tell $
            fromAccessor
              ( Accessor
                  { bufferView = pure bufferViewIndex,
                    byteOffset = pure accessorByteOffset,
                    componentType,
                    count = elemCount accessorData,
                    name = Nothing,
                    accessorType,
                    max = floatsToDoubles <$> getMax,
                    min = floatsToDoubles <$> getMin
                  }
              )
              bytes
          return accessorIndexOffset
    where
      accessorState byteLength st@(EncodingState {accessorIndexOffset, accessorByteOffset}) =
        st
          { accessorIndexOffset = accessorIndexOffset + 1,
            accessorByteOffset = accessorByteOffset + fromIntegral byteLength
          }
      getAccessorTypes = case accessorData of
        (Vec3Float _) -> ("VEC3", 5126)
        (Vec2Float _) -> ("VEC2", 5126)
        (ScalarShort _) -> ("SCALAR", 5123)
      getMin = case accessorData of
        (Vec3Float xs) -> getVectorMin xs
        (Vec2Float xs) -> getVectorMin xs
        (ScalarShort xs) -> getScalarMin xs
        where
          getVectorMin :: (MonadZip v, Foldable v, Ord a) => Vector (v a) -> Maybe [a]
          getVectorMin = pure . toList . mzipMin
          getScalarMin :: (Integral a, Num b) => Vector a -> Maybe [b]
          getScalarMin = pure . singleton . fromIntegral . minimum
      floatsToDoubles = map floatToDouble
      getMax = case accessorData of
        (Vec3Float xs) -> getVectorMax xs
        (Vec2Float xs) -> getVectorMax xs
        (ScalarShort xs) -> getScalarMax xs
        where
          getVectorMax :: (MonadZip v, Foldable v, Ord a) => Vector (v a) -> Maybe [a]
          getVectorMax = pure . toList . mzipMax
          getScalarMax :: (Integral a, Num b) => Vector a -> Maybe [b]
          getScalarMax = pure . singleton . fromIntegral . maximum

stride :: AccessorData -> Int
stride (Vec3Float _) = 12
stride (Vec2Float _) = 8
stride (ScalarShort _) = 2

byteSize :: AccessorData -> Int
byteSize (Vec3Float xs) = 12 * V.length xs
byteSize (Vec2Float xs) = 8 * V.length xs
byteSize (ScalarShort xs) = 2 * V.length xs

elemCount :: AccessorData -> Int
elemCount (Vec3Float xs) = V.length xs
elemCount (Vec2Float xs) = V.length xs
elemCount (ScalarShort xs) = V.length xs

encodeAccessorData :: AccessorData -> ByteString
encodeAccessorData = runPut . putAccessorData
  where
    putAccessorData :: AccessorData -> Put
    putAccessorData (Vec3Float vector) = putVec3Array putFloatle vector
    putAccessorData (Vec2Float vector) = putVec2Array putFloatle vector
    putAccessorData (ScalarShort vector) = putScalarArray putWord16le vector

    putV3 :: (a -> Put) -> V3 a -> Put
    putV3 p (V3 x y z) = p x <> p y <> p z

    putVec3Array :: (a -> Put) -> Vector (V3 a) -> Put
    putVec3Array = V.mapM_ . putV3

    putV2 :: (a -> Put) -> V2 a -> Put
    putV2 p (V2 x y) = p x <> p y

    putVec2Array :: (a -> Put) -> Vector (V2 a) -> Put
    putVec2Array = V.mapM_ . putV2

    putScalarArray :: (a -> Put) -> Vector a -> Put
    putScalarArray = V.mapM_
