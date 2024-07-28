{-# LANGUAGE DuplicateRecordFields #-}

module Core.Model (module Core.Model) where

import Data.Map (Map)
import Data.Word (Word16)

import Linear (M44, V3, V2)

data VertexArray =
    Vec3Array [V3 Float] |
    Vec2Array [V2 Float]
    deriving (Eq, Show)

newtype IndexArray =
    ShortIndex [Word16]
    deriving (Eq, Show)

data Material = Material
    deriving (Eq, Show)

data Mesh = Mesh {
    name :: Maybe String,
    primitives :: [Primitive]
} deriving (Eq, Show)

data Attribute =
    Position |
    Normal |
    TexCoord Int
    deriving (Eq, Show)

data Primitive = Primitive {
    attributes :: Map Attribute VertexArray,
    indices :: Maybe IndexArray,
    material :: Maybe Material
} deriving (Eq, Show)

data Node = Node {
    children :: [Node],
    matrix :: M44 Float,
    name :: Maybe String,
    mesh :: Maybe Mesh
} deriving (Eq, Show)

data Scene = Scene {
    name :: Maybe String,
    nodes :: [Node]
} deriving (Eq, Show)

scene :: String -> [Node] -> Scene
scene name = Scene (Just name)
