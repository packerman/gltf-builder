module Lib.Image
  ( generateRGB8Png,
    solidColor,
    horizontalGradient,
    PixelRGB8 (..),
  )
where

import Codec.Picture
  ( DynamicImage (..),
    Pixel8,
    PixelRGB8 (..),
    generateImage,
  )
import Codec.Picture.Saving
import Data.ByteString.Lazy as BSL
import Linear

generateRGB8Png :: (Int -> Int -> PixelRGB8) -> Int -> Int -> ByteString
generateRGB8Png f width = imageToPng . ImageRGB8 . generateImage f width

solidColor :: PixelRGB8 -> Int -> Int -> ByteString
solidColor c = generateRGB8Png (\_ _ -> c)

horizontalGradient :: PixelRGB8 -> PixelRGB8 -> Int -> Int -> ByteString
horizontalGradient lowColor highColor width height =
  generateRGB8Png
    ( \_ y ->
        let low = rgb8toV3 lowColor
            hight = rgb8toV3 highColor
            s = fromIntegral y / fromIntegral (height - 1)
         in v3ToRgb8 $ lerp s low hight
    )
    width
    height

maxPixelValue :: (Num a) => a
maxPixelValue = fromIntegral (maxBound :: Pixel8)

rgb8toV3 :: PixelRGB8 -> V3 Double
rgb8toV3 (PixelRGB8 r g b) =
  V3
    (fromIntegral r / maxPixelValue)
    (fromIntegral g / maxPixelValue)
    (fromIntegral b / maxPixelValue)

v3ToRgb8 :: V3 Double -> PixelRGB8
v3ToRgb8 (V3 r g b) =
  PixelRGB8
    (round $ r * maxPixelValue)
    (round $ g * maxPixelValue)
    (round $ b * maxPixelValue)
