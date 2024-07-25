{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Gltf (
    module Gltf,
    module Data.Aeson
) where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

type Number = Double
type Index = Int

data Gltf = Gltf {
    asset :: Asset,
    nodes :: Maybe [Node],
    scene :: Maybe Index,
    scenes :: Maybe [Scene]
} deriving (Generic, Show)

customOptions :: Options
customOptions = defaultOptions {
    omitNothingFields = True,
    rejectUnknownFields = True
}

instance ToJSON Gltf where
    toEncoding = genericToEncoding customOptions

instance FromJSON Gltf

readGltf :: FilePath -> IO (Either String Gltf)
readGltf path = eitherDecode <$> BSL.readFile path

data Asset = Asset {
    generator :: Maybe String,
    version :: String
} deriving (Generic, Show)

instance ToJSON Asset where
    toEncoding = genericToEncoding customOptions

instance FromJSON Asset

data Node = Node {
    children :: Maybe [Index],
    matrix :: Maybe [Number],
    mesh :: Maybe Index,
    name :: Maybe String
} deriving (Generic, Show)

instance ToJSON Node where
    toEncoding = genericToEncoding customOptions

instance FromJSON Node

data Scene = Scene {
    name :: Maybe String,
    nodes :: Maybe [Index]
} deriving (Generic, Show)

instance ToJSON Scene where
    toEncoding = genericToEncoding customOptions

instance FromJSON Scene
