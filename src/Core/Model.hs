{-# LANGUAGE DuplicateRecordFields #-}

module Core.Model (module Core.Model) where

import Data.Map (Map)

import Linear (M44, identity)
import Data.Attribute (AttributeData)
import Data.Index (IndexData)

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
    deriving (Eq, Show, Ord)

data Mode =
    Points |
    Lines |
    LineLoop |
    LineStrip |
    Triangles |
    TriangleStrip |
    TriangleFan
    deriving (Eq, Show)

data Primitive = Primitive {
    attributes :: Map Attribute AttributeData,
    indices :: Maybe IndexData,
    material :: Maybe Material,
    mode :: Mode
} deriving (Eq, Show)

data Node = Node {
    children :: [Node],
    matrix :: M44 Float,
    name :: Maybe String,
    mesh :: Maybe Mesh
} deriving (Eq, Show)

defaultNode :: Node
defaultNode = Node {
    children = [],
    matrix = identity,
    name = Nothing,
    mesh = Nothing
}

data Scene = Scene {
    name :: Maybe String,
    nodes :: [Node]
} deriving (Eq, Show)

scene :: Maybe String -> [Node] -> Scene
scene = Scene
