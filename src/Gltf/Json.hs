{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Gltf.Json (
    module Gltf.Json,
    module Data.Aeson
) where

import GHC.Generics
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector as V

type Number = Double
type Index = Int

type GltfList a = Maybe (Vector a)

gltfList :: [a] -> GltfList a
gltfList [] = Nothing
gltfList xs = Just $ V.fromList xs

data Gltf = Gltf {
    accessors :: GltfList Accessor,
    asset :: Asset,
    buffers :: GltfList Buffer,
    bufferViews :: GltfList BufferView,
    images :: GltfList Image,
    materials :: GltfList Material,
    meshes :: GltfList Mesh,
    nodes :: GltfList Node,
    samplers :: GltfList Sampler,
    scene :: Maybe Index,
    scenes :: GltfList Scene,
    textures :: GltfList Texture
} deriving (Generic, Show)

defaultGltf :: Gltf
defaultGltf = Gltf {
    accessors = Nothing,
    asset = defaultAsset,
    buffers = Nothing,
    bufferViews = Nothing,
    images = Nothing,
    materials = Nothing,
    meshes = Nothing,
    nodes = Nothing,
    samplers = Nothing,
    scene = Nothing,
    scenes = Nothing,
    textures = Nothing
}

readOptions :: Options
readOptions = defaultOptions {
    rejectUnknownFields = True
}

writeOptions :: Options
writeOptions = defaultOptions {
    omitNothingFields = True
}

instance ToJSON Gltf where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Gltf where
    parseJSON = genericParseJSON readOptions

data Accessor = Accessor {
    bufferView :: Maybe Index,
    byteOffset :: Maybe Index,
    componentType :: Index,
    count :: Index,
    name :: Maybe String,
    accessorType :: String,
    max :: Maybe [Number],
    min :: Maybe [Number]
} deriving (Generic, Show)

instance ToJSON Accessor where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Accessor where
    parseJSON = genericParseJSON readOptions {
        fieldLabelModifier =
            \label -> case label of  
                        "accessorType" -> "type"
                        _ -> label
    }

data Asset = Asset {
    generator :: Maybe String,
    version :: String
} deriving (Generic, Show)

defaultAsset :: Asset
defaultAsset = Asset {
    version = "2.0",
    generator = Nothing
}

instance ToJSON Asset where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Asset where
    parseJSON = genericParseJSON readOptions

data Buffer = Buffer {
    byteLength :: Index,
    name :: Maybe String,
    uri :: Maybe Text
} deriving (Generic, Show)

instance ToJSON Buffer where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Buffer where
    parseJSON = genericParseJSON readOptions

data BufferView = BufferView {
    buffer :: Index,
    byteOffset :: Maybe Index,
    byteLength :: Index,
    byteStride :: Maybe Index,
    name :: Maybe String,
    target :: Maybe Index
} deriving (Generic, Show)

instance ToJSON BufferView where
    toEncoding = genericToEncoding writeOptions

instance FromJSON BufferView where
    parseJSON = genericParseJSON readOptions

data Image = Image {
    name :: Maybe String,
    uri :: Maybe Text
} deriving (Generic, Show)

defaultImage :: Image
defaultImage = Image {
    name = Nothing,
    uri = Nothing
}

instance ToJSON Image where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Image where
    parseJSON = genericParseJSON readOptions

data Material = Material {
    name :: Maybe String,
    pbrMetallicRoughness :: Maybe PbrMetallicRoughness
} deriving (Generic, Show)

defaultMaterial :: Material
defaultMaterial = Material {
    name = Nothing,
    pbrMetallicRoughness = Nothing
}

instance ToJSON Material where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Material where
    parseJSON = genericParseJSON readOptions

data PbrMetallicRoughness = PbrMetallicRoughness {
    baseColorFactor :: Maybe [Number],
    baseColorTexture :: Maybe TextureInfo,
    metallicFactor :: Maybe Number
} deriving (Generic, Show)

defaultPbrMetallicRoughness :: PbrMetallicRoughness
defaultPbrMetallicRoughness = PbrMetallicRoughness {
    baseColorFactor = Nothing,
    baseColorTexture = Nothing,
    metallicFactor = Nothing
}

instance ToJSON PbrMetallicRoughness where
    toEncoding = genericToEncoding writeOptions

instance FromJSON PbrMetallicRoughness where
    parseJSON = genericParseJSON readOptions

data Mesh = Mesh {
    name :: Maybe String,
    primitives :: Maybe [Primitive]
} deriving (Generic, Show)

defaultMesh :: Mesh
defaultMesh = Mesh {
    name = Nothing,
    primitives = Nothing
}

instance ToJSON Mesh where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Mesh where
    parseJSON = genericParseJSON readOptions

data Primitive = Primitive {
    attributes :: Map String Index,
    indices ::  Maybe Index,
    material :: Maybe Index,
    mode :: Maybe Index
} deriving (Generic, Show)

instance ToJSON Primitive where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Primitive where
    parseJSON = genericParseJSON readOptions

data Node = Node {
    children :: Maybe [Index],
    matrix :: Maybe [Number],
    mesh :: Maybe Index,
    name :: Maybe String
} deriving (Generic, Show)

defaultNode :: Node
defaultNode = Node {
    children = Nothing,
    matrix = Nothing,
    mesh = Nothing,
    name = Nothing
}

instance ToJSON Node where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Node where
    parseJSON = genericParseJSON readOptions

data Sampler = Sampler {
    magFilter :: Maybe Index,
    minFilter :: Maybe Index,
    name :: Maybe String,
    wrapS :: Maybe Index,
    wrapT :: Maybe Index
} deriving (Generic, Show)

defaultSampler :: Sampler
defaultSampler = Sampler {
    magFilter = Nothing,
    minFilter = Nothing,
    name = Nothing,
    wrapS = Nothing,
    wrapT = Nothing
}

instance ToJSON Sampler where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Sampler where
    parseJSON = genericParseJSON readOptions

data Scene = Scene {
    name :: Maybe String,
    nodes :: Maybe [Index]
} deriving (Generic, Show)

defaultScene :: Scene
defaultScene = Scene {
    name = Nothing,
    nodes = Nothing
}

instance ToJSON Scene where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Scene where
    parseJSON = genericParseJSON readOptions

data Texture = Texture {
    name :: Maybe String,
    sampler :: Maybe Index,
    source :: Maybe Index
} deriving (Generic, Show)

defaultTexture :: Texture
defaultTexture = Texture {
    name = Nothing,
    sampler = Nothing,
    source = Nothing
}

instance ToJSON Texture where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Texture where
    parseJSON = genericParseJSON readOptions

data TextureInfo = TextureInfo {
    index :: Index,
    texCoord :: Maybe Index
} deriving (Generic, Show)

instance ToJSON TextureInfo where
    toEncoding = genericToEncoding writeOptions

instance FromJSON TextureInfo where
    parseJSON = genericParseJSON readOptions
