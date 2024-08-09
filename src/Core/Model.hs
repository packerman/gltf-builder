{-# LANGUAGE DuplicateRecordFields #-}

module Core.Model (module Core.Model) where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16)
import Linear (M44, V3, identity)

data Material = Material
  deriving (Eq, Show)

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

newtype AttributeData = Vec3Attribute (Vector (V3 Float))
  deriving (Eq, Show)

fromV3List :: [V3 Float] -> AttributeData
fromV3List = Vec3Attribute . V.fromList

newtype IndexData = ShortIndex (Vector Word16)
  deriving (Eq, Show)

data Primitive = Primitive
  { attributes :: Map Attribute AttributeData,
    indices :: Maybe IndexData,
    material :: Maybe Material,
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
