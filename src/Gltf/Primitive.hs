module Gltf.Primitive (encodePrimitive) where

import Control.Monad
import Control.Monad.Trans.State
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
import Gltf.Json (Accessor (..), BufferView (..))
import Gltf.Primitive.Types
import qualified Gltf.Primitive.Types as EncAcc (EncodedAccessor (..))
import qualified Gltf.Primitive.Types as EncInd (EncodedIndices (..))
import qualified Gltf.Primitive.Types as EncStrGrp (EncodedStrideGroup (..))
import Lib.Base (mcons, mzipMax, mzipMin)
import Lib.Container (groupBy)
import Linear (V2 (..), V3 (..))

data EncodedPrimitive = EncodedPrimitive
  { attributes :: Map String Int,
    indices :: Maybe Int,
    bytes :: [ByteString],
    accessors :: [Accessor],
    bufferViews :: [BufferView]
  }
  deriving (Eq, Show)

data EncodingState = EncodingState
  { accessorIndexOffset :: Int,
    bufferIndex :: Int,
    bufferViewIndex :: Int,
    bufferViewByteOffset :: Int,
    accessorByteOffset :: Int
  }

type EncodingM = State EncodingState

encodePrimitive ::
  Map String AccessorData ->
  Maybe AccessorData ->
  EncodingM EncodedPrimitive
encodePrimitive attributes indices = do
  encodedIndices <- traverse encodeIndices indices
  encodedAttributes <- encodeAttributes attributes
  return
    EncodedPrimitive
      { attributes = attributeAccessorIndices encodedAttributes,
        indices = accessIndex accessorIndex <$> encodedIndices,
        bytes =
          mcons
            (accessIndex accessorBytes <$> encodedIndices)
            (accessAttribute accessorBytes encodedAttributes),
        accessors =
          mcons
            (accessIndex EncAcc.accessor <$> encodedIndices)
            (accessAttribute EncAcc.accessor encodedAttributes),
        bufferViews =
          mcons
            (EncInd.bufferView <$> encodedIndices)
            (EncStrGrp.bufferView <$> encodedAttributes)
      }
  where
    attributeAccessorIndices =
      M.unions
        . map (\(EncodedStrideGroup {attributes = attrs}) -> M.map accessorIndex attrs)
    accessIndex f (EncodedIndices {accessor}) = f accessor
    accessAttribute f = (>>= (\(EncodedStrideGroup {attributes = attrs}) -> map f $ M.elems attrs))

encodeAttributes :: Map String AccessorData -> EncodingM [EncodedStrideGroup]
encodeAttributes attributes =
  let -- attributeCount = fromJust getAttributeCount
      strideGroups = groupBy (stride . snd) (M.assocs attributes)
      byteSizeSum = sum $ byteSize <$> M.elems attributes
   in forM (M.elems strideGroups) (encodeWithStride byteSizeSum)
  where
    -- getAttributeCount :: Maybe Int
    -- getAttributeCount = getSingleWith (elemCount . snd) (M.assocs attributes)
    --   where
    --     getSingle :: (Eq a) => [a] -> Maybe a
    --     getSingle (x : xs) = if all (== x) xs then Just x else Nothing
    --     getSingle _ = Nothing
    --     getSingleWith :: (Eq b) => (a -> b) -> [a] -> Maybe b
    --     getSingleWith f = getSingle . map f
    encodeWithStride :: Int -> [(String, AccessorData)] -> EncodingM EncodedStrideGroup
    encodeWithStride byteSizeSum attributeList = do
      resetAccessorByteOffset
      attrs <- forM attributeList (mapM encodeAccessor)
      bufferView <- createBufferView Nothing (pure 34962) byteSizeSum
      return
        EncodedStrideGroup
          { attributes = M.fromList attrs,
            bufferView
          }

-- createBuffer :: Buffer
-- createBuffer =
--   Buffer
--     { byteLength = undefined,
--       name = Nothing,
--       uri = undefined
--     }

encodeIndices :: AccessorData -> EncodingM EncodedIndices
encodeIndices accessorData =
  do
    resetAccessorByteOffset
    accessor <- encodeAccessor accessorData
    bufferView <- createElementArrayBuffer (byteSize accessorData)
    return EncodedIndices {accessor, bufferView}
  where
    createElementArrayBuffer = createBufferView Nothing (pure 34963)

createBufferView :: Maybe Int -> Maybe Int -> Int -> EncodingM BufferView
createBufferView byteStride target byteLength = do
  (EncodingState {bufferIndex, bufferViewByteOffset, bufferViewIndex}) <- get
  modify
    ( \s ->
        s
          { bufferViewIndex = bufferViewIndex + 1,
            bufferViewByteOffset = bufferViewByteOffset + byteLength
          }
    )
  return
    BufferView
      { buffer = bufferIndex,
        byteOffset = pure bufferViewByteOffset,
        byteLength,
        byteStride,
        name = Nothing,
        target
      }

resetAccessorByteOffset :: EncodingM ()
resetAccessorByteOffset = modify (\s -> s {accessorByteOffset = 0})

encodeAccessor :: AccessorData -> EncodingM EncodedAccessor
encodeAccessor
  accessorData =
    let (accessorType, componentType) = getAccessorTypes
        bytes = encodeAccessorData accessorData
     in do
          ( EncodingState
              { bufferViewIndex,
                accessorByteOffset,
                accessorIndexOffset,
                bufferViewByteOffset
              }
            ) <-
            get
          modify
            ( \s ->
                s
                  { accessorIndexOffset = accessorIndexOffset + 1,
                    accessorByteOffset = accessorByteOffset + fromIntegral (BSL.length bytes),
                    bufferViewByteOffset = bufferViewByteOffset + fromIntegral (BSL.length bytes)
                  }
            )
          return
            EncodedAccessor
              { accessorBytes = bytes,
                accessorIndex = accessorIndexOffset,
                accessor =
                  Accessor
                    { bufferView = pure bufferViewIndex,
                      byteOffset = pure accessorByteOffset,
                      componentType,
                      count = elemCount accessorData,
                      name = Nothing,
                      accessorType,
                      max = getMax,
                      min = getMin
                    }
              }
    where
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
