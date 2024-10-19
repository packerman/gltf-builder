{-# LANGUAGE DeriveGeneric #-}

module Gltf.Json
  ( module Gltf.Json,
    module Data.Aeson,
  )
where

import Control.Applicative
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    eitherDecode,
    genericParseJSON,
    genericToEncoding,
  )
import Data.Function
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import Gltf.Array (Array)
import Data.Default

type Number = Float

type Index = Int

data Gltf = Gltf
  { accessors :: Array Accessor,
    asset :: Asset,
    buffers :: Array Buffer,
    bufferViews :: Array BufferView,
    images :: Array Image,
    materials :: Array Material,
    meshes :: Array Mesh,
    nodes :: Array Node,
    samplers :: Array Sampler,
    scene :: Maybe Index,
    scenes :: Array Scene,
    textures :: Array Texture
  }
  deriving (Generic, Eq, Show)

instance Default Gltf where
  def = Gltf
    { accessors = Nothing,
      asset = def,
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
readOptions =
  defaultOptions
    { rejectUnknownFields = True
    }

writeOptions :: Options
writeOptions =
  defaultOptions
    { omitNothingFields = True
    }

instance ToJSON Gltf where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Gltf where
  parseJSON = genericParseJSON readOptions

data Accessor = Accessor
  { bufferView :: Maybe Index,
    byteOffset :: Maybe Index,
    componentType :: Index,
    count :: Index,
    name :: Maybe String,
    accessorType :: String,
    max :: Maybe [Number],
    min :: Maybe [Number]
  }
  deriving (Generic, Eq, Show)

instance ToJSON Accessor where
  toEncoding =
    genericToEncoding
      writeOptions
        { fieldLabelModifier = accessorFieldLabelModifier
        }

instance FromJSON Accessor where
  parseJSON =
    genericParseJSON
      readOptions
        { fieldLabelModifier = accessorFieldLabelModifier
        }

accessorFieldLabelModifier :: String -> String
accessorFieldLabelModifier label = case label of
  "accessorType" -> "type"
  _ -> label

data Asset = Asset
  { generator :: Maybe String,
    version :: String
  }
  deriving (Generic, Eq, Show)

instance Default Asset where
  def = Asset
    { version = "2.0",
      generator = Nothing
    }

instance ToJSON Asset where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Asset where
  parseJSON = genericParseJSON readOptions

data Buffer = Buffer
  { byteLength :: Index,
    name :: Maybe String,
    uri :: Maybe Text
  }
  deriving (Generic, Eq, Show)

instance ToJSON Buffer where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Buffer where
  parseJSON = genericParseJSON readOptions

data BufferView = BufferView
  { buffer :: Index,
    byteOffset :: Maybe Index,
    byteLength :: Index,
    byteStride :: Maybe Index,
    name :: Maybe String,
    target :: Maybe Index
  }
  deriving (Generic, Eq, Show)

instance ToJSON BufferView where
  toEncoding = genericToEncoding writeOptions

instance FromJSON BufferView where
  parseJSON = genericParseJSON readOptions

data Image = Image
  { name :: Maybe String,
    uri :: Maybe Text
  }
  deriving (Generic, Eq, Show)

defaultImage :: Image
defaultImage =
  Image
    { name = Nothing,
      uri = Nothing
    }

instance ToJSON Image where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Image where
  parseJSON = genericParseJSON readOptions

data Material = Material
  { name :: Maybe String,
    pbrMetallicRoughness :: Maybe PbrMetallicRoughness,
    alphaMode :: Maybe String,
    alphaCutoff :: Maybe Number,
    doubleSided :: Maybe Bool
  }
  deriving (Generic, Eq, Show)

defaultAlphaMode :: String
defaultAlphaMode = "OPAQUE"

defaultAlphaCutoff :: Number
defaultAlphaCutoff = 0.5

defaultDoubleSided :: Bool
defaultDoubleSided = False

defaultMaterial :: Material
defaultMaterial =
  Material
    { name = Nothing,
      pbrMetallicRoughness = Nothing,
      doubleSided = pure defaultDoubleSided,
      alphaMode = pure defaultAlphaMode,
      alphaCutoff = pure defaultAlphaCutoff
    }

instance ToJSON Material where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Material where
  parseJSON = genericParseJSON readOptions

data PbrMetallicRoughness = PbrMetallicRoughness
  { baseColorFactor :: Maybe [Number],
    baseColorTexture :: Maybe TextureInfo,
    metallicFactor :: Maybe Number,
    roughnessFactor :: Maybe Number,
    metallicRoughnessTexture :: Maybe TextureInfo
  }
  deriving (Generic, Eq, Show)

defaultPbrMetallicRoughness :: PbrMetallicRoughness
defaultPbrMetallicRoughness =
  PbrMetallicRoughness
    { baseColorFactor = Just [1, 1, 1, 1],
      baseColorTexture = Nothing,
      metallicFactor = Just 1,
      roughnessFactor = Just 1,
      metallicRoughnessTexture = Nothing
    }

instance Semigroup PbrMetallicRoughness where
  m1 <> m2 =
    PbrMetallicRoughness
      { baseColorFactor = on (<|>) baseColorFactor m1 m2,
        baseColorTexture = on (<|>) baseColorTexture m1 m2,
        metallicFactor = on (<|>) metallicFactor m1 m2,
        roughnessFactor = on (<|>) roughnessFactor m1 m2,
        metallicRoughnessTexture = on (<|>) metallicRoughnessTexture m1 m2
      }

instance ToJSON PbrMetallicRoughness where
  toEncoding = genericToEncoding writeOptions

instance FromJSON PbrMetallicRoughness where
  parseJSON = genericParseJSON readOptions

data Mesh = Mesh
  { name :: Maybe String,
    primitives :: [Primitive]
  }
  deriving (Generic, Eq, Show)

instance ToJSON Mesh where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Mesh where
  parseJSON = genericParseJSON readOptions

data Primitive = Primitive
  { attributes :: Map String Index,
    indices :: Maybe Index,
    material :: Maybe Index,
    mode :: Maybe Index
  }
  deriving (Generic, Eq, Show)

instance ToJSON Primitive where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Primitive where
  parseJSON = genericParseJSON readOptions

data Node = Node
  { children :: Maybe [Index],
    matrix :: Maybe [Number],
    mesh :: Maybe Index,
    name :: Maybe String
  }
  deriving (Generic, Eq, Show)

defaultNode :: Node
defaultNode =
  Node
    { children = Nothing,
      matrix = Nothing,
      mesh = Nothing,
      name = Nothing
    }

instance ToJSON Node where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Node where
  parseJSON = genericParseJSON readOptions

data Sampler = Sampler
  { magFilter :: Maybe Index,
    minFilter :: Maybe Index,
    name :: Maybe String,
    wrapS :: Maybe Index,
    wrapT :: Maybe Index
  }
  deriving (Generic, Eq, Show)

defaultSampler :: Sampler
defaultSampler =
  Sampler
    { magFilter = Nothing,
      minFilter = Nothing,
      name = Nothing,
      wrapS = Nothing,
      wrapT = Nothing
    }

instance ToJSON Sampler where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Sampler where
  parseJSON = genericParseJSON readOptions

data Scene = Scene
  { name :: Maybe String,
    nodes :: Maybe [Index]
  }
  deriving (Generic, Eq, Show)

defaultScene :: Scene
defaultScene =
  Scene
    { name = Nothing,
      nodes = Nothing
    }

instance ToJSON Scene where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Scene where
  parseJSON = genericParseJSON readOptions

data Texture = Texture
  { name :: Maybe String,
    sampler :: Maybe Index,
    source :: Maybe Index
  }
  deriving (Generic, Eq, Show)

defaultTexture :: Texture
defaultTexture =
  Texture
    { name = Nothing,
      sampler = Nothing,
      source = Nothing
    }

instance ToJSON Texture where
  toEncoding = genericToEncoding writeOptions

instance FromJSON Texture where
  parseJSON = genericParseJSON readOptions

data TextureInfo = TextureInfo
  { index :: Index,
    texCoord :: Maybe Index
  }
  deriving (Generic, Eq, Show)

instance ToJSON TextureInfo where
  toEncoding = genericToEncoding writeOptions

instance FromJSON TextureInfo where
  parseJSON = genericParseJSON readOptions
