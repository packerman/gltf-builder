{-# LANGUAGE DuplicateRecordFields #-}

module Core.Model (module Core.Model) where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16)
import Linear (M44, V2, V3, V4 (..), identity)

data Material = Material
  { name :: Maybe String,
    pbrMetallicRoughness :: PbrMetallicRoughness
  }
  deriving (Eq, Show)

data PbrMetallicRoughness = PbrMetallicRoughness
  { baseColorFactor :: V4 Float,
    baseColorTexture :: Maybe Texture,
    metallicFactor :: Float,
    roughnessFactor :: Float,
    metallicRoughnessTexture :: Maybe Texture
  }
  deriving (Eq, Show)

defaultMaterial :: Material
defaultMaterial =
  Material
    { name = Nothing,
      pbrMetallicRoughness =
        PbrMetallicRoughness
          { baseColorFactor = V4 1 1 1 1,
            baseColorTexture = Nothing,
            metallicFactor = 1,
            roughnessFactor = 1,
            metallicRoughnessTexture = Nothing
          }
    }

data Mesh = Mesh
  { name :: Maybe String,
    primitives :: [Primitive]
  }
  deriving (Eq, Show)

data Attribute
  = Position
  | Normal
  | TexCoord Int
  deriving (Eq, Show, Ord)

data Mode
  = Points
  | Lines
  | LineLoop
  | LineStrip
  | Triangles
  | TriangleStrip
  | TriangleFan
  deriving (Eq, Show)

data AttributeData
  = Vec3Attribute (Vector (V3 Float))
  | Vec2Attribute (Vector (V2 Float))
  deriving (Eq, Show)

fromV3List :: [V3 Float] -> AttributeData
fromV3List = Vec3Attribute . V.fromList

vec2Attribute :: Vector (V2 Float) -> AttributeData
vec2Attribute = Vec2Attribute

vec3Attribute :: Vector (V3 Float) -> AttributeData
vec3Attribute = Vec3Attribute

newtype IndexData = ShortIndex (Vector Word16)
  deriving (Eq, Show)

shortIndex :: Vector Word16 -> IndexData
shortIndex = ShortIndex

data Primitive = Primitive
  { attributes :: Map Attribute AttributeData,
    indices :: Maybe IndexData,
    material :: Material,
    mode :: Mode
  }
  deriving (Eq, Show)

data Node = Node
  { children :: [Node],
    matrix :: M44 Float,
    name :: Maybe String,
    mesh :: Maybe Mesh
  }
  deriving (Eq, Show)

defaultNode :: Node
defaultNode =
  Node
    { children = [],
      matrix = identity,
      name = Nothing,
      mesh = Nothing
    }

data Scene = Scene
  { name :: Maybe String,
    nodes :: [Node]
  }
  deriving (Eq, Show)

scene :: Maybe String -> [Node] -> Scene
scene = Scene

data Texture = Texture
  { image :: Image,
    sampler :: Sampler
  }
  deriving (Eq, Show)

data Image = Image deriving (Eq, Show)

data Sampler = Sampler deriving (Eq, Show)
