module Core.Translate.Enums (module Core.Translate.Enums) where

import Core.Model
  ( Alpha (..),
    Attribute (..),
    MagFilter (..),
    MinFilter (..),
    Mode (..),
    Wrap (..),
  )
import Data.Maybe (fromMaybe)
import Gltf.Json (Number, defaultAlphaCutoff)
import Lib.Base (nothingIf)

decodeMode :: (Real a, Show a) => a -> Either String Mode
decodeMode n = case n of
  0 -> pure Points
  1 -> pure Lines
  2 -> pure LineLoop
  3 -> pure LineStrip
  4 -> pure Triangles
  5 -> pure TriangleStrip
  6 -> pure TriangleFan
  _ -> Left $ unwords ["Unknown mode:", show n]

encodeMode :: (Num a) => Mode -> a
encodeMode Points = 0
encodeMode Lines = 1
encodeMode LineLoop = 2
encodeMode LineStrip = 3
encodeMode Triangles = 4
encodeMode TriangleStrip = 5
encodeMode TriangleFan = 6

decodeAttribute :: String -> Either String Attribute
decodeAttribute key = case key of
  "POSITION" -> pure Position
  "NORMAL" -> pure Normal
  "TEXCOORD_0" -> pure $ TexCoord 0
  _ -> Left $ unwords ["Unknown attribute: ", key]

encodeAttribute :: Attribute -> String
encodeAttribute Position = "POSITION"
encodeAttribute (TexCoord n) = "TEXCOORD_" <> show n
encodeAttribute Normal = "NORMAL"

decodeMagFilter :: (Real a, Show a) => a -> Either String MagFilter
decodeMagFilter n = case n of
  9728 -> pure MagNearest
  9729 -> pure MagLinear
  _ -> Left $ unwords ["Unknown mag filter", show n]

decodeMinFilter :: (Real a, Show a) => a -> Either String MinFilter
decodeMinFilter n = case n of
  9728 -> pure MinNearest
  9729 -> pure MinLinear
  9984 -> pure NearestMipMapNearest
  9985 -> pure LinearMipmapNearest
  9986 -> pure NearestMipmapLinear
  9987 -> pure LinearMipmapLinear
  _ -> Left $ unwords ["Unknown min filter", show n]

encodeMagFilter :: (Num a) => MagFilter -> a
encodeMagFilter f = case f of
  MagNearest -> 9728
  MagLinear -> 9729

encodeMinFilter :: (Num a) => MinFilter -> a
encodeMinFilter f = case f of
  MinNearest -> 9728
  MinLinear -> 9729
  NearestMipMapNearest -> 9984
  LinearMipmapNearest -> 9985
  NearestMipmapLinear -> 9986
  LinearMipmapLinear -> 9987

decodeWrap :: (Real a, Show a) => a -> Either String Wrap
decodeWrap n = case n of
  33071 -> pure ClampToEdge
  33648 -> pure MirroredRepeat
  10497 -> pure Repeat
  _ -> Left $ unwords ["Unknown wrap mode", show n]

encodeWrap :: (Num a) => Wrap -> a
encodeWrap w = case w of
  ClampToEdge -> 33071
  MirroredRepeat -> 33648
  Repeat -> 10497

decodeAlpha :: Maybe String -> Maybe Double -> Either String Alpha
decodeAlpha (Just "OPAQUE") _ = pure Opaque
decodeAlpha (Just "MASK") alphaCutoff = pure $ Mask $ fromMaybe defaultAlphaCutoff alphaCutoff
decodeAlpha (Just "BLEND") _ = pure Blend
decodeAlpha (Just mode) _ = Left $ unwords ["Unknown alpha mode:", mode]
decodeAlpha Nothing (Just _) = Left "Alpha cutoff is not allowed"
decodeAlpha _ _ = pure Opaque

encodeAlphaMode :: Alpha -> (Maybe String, Maybe Number)
encodeAlphaMode Opaque = (Nothing, Nothing)
encodeAlphaMode (Mask alphaCutoff) = (pure "MASK", nothingIf (== defaultAlphaCutoff) alphaCutoff)
encodeAlphaMode Blend = (pure "BLEND", Nothing)
