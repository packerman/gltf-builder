{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Gltf (
    module Gltf,
    module Data.Aeson
) where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)

type Number = Double
type Index = Int

data Gltf = Gltf {
    accessors :: Maybe [Accessor],
    asset :: Asset,
    buffers :: Maybe [Buffer],
    bufferViews :: Maybe[BufferView],
    materials :: Maybe [Material],
    meshes :: Maybe [Mesh],
    nodes :: Maybe [Node],
    scene :: Maybe Index,
    scenes :: Maybe [Scene]
} deriving (Generic, Show)

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

readGltf :: FilePath -> IO (Either String Gltf)
readGltf path = eitherDecode <$> BSL.readFile path

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

instance ToJSON Asset where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Asset where
    parseJSON = genericParseJSON readOptions

data Buffer = Buffer {
    byteLength :: Index,
    name :: Maybe String,
    uri :: Maybe String
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

data Material = Material {
    name :: Maybe String,
    pbrMetallicRoughness :: PbrMetallicRoughness
} deriving (Generic, Show)

instance ToJSON Material where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Material where
    parseJSON = genericParseJSON readOptions

data PbrMetallicRoughness = PbrMetallicRoughness {
    baseColorFactor :: Maybe [Number],
    metallicFactor :: Maybe Number
} deriving (Generic, Show)

instance ToJSON PbrMetallicRoughness where
    toEncoding = genericToEncoding writeOptions

instance FromJSON PbrMetallicRoughness where
    parseJSON = genericParseJSON readOptions

data Mesh = Mesh {
    name :: Maybe String,
    primitives :: Maybe [Primitive]
} deriving (Generic, Show)

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

instance ToJSON Node where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Node where
    parseJSON = genericParseJSON readOptions

data Scene = Scene {
    name :: Maybe String,
    nodes :: Maybe [Index]
} deriving (Generic, Show)

instance ToJSON Scene where
    toEncoding = genericToEncoding writeOptions

instance FromJSON Scene where
    parseJSON = genericParseJSON readOptions
