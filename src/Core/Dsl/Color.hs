module Core.Dsl.Color (module Core.Dsl.Color) where

import Codec.Picture

black :: PixelRGB8
black = PixelRGB8 0 0 0

white :: PixelRGB8
white = PixelRGB8 255 255 255

red :: PixelRGB8
red = PixelRGB8 255 0 0

green :: PixelRGB8
green = PixelRGB8 0 255 0

blue :: PixelRGB8
blue = PixelRGB8 0 0 255

yellow :: PixelRGB8
yellow = PixelRGB8 255 255 0
