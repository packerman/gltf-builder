module Options (module Options) where

import Data.Default
import Gltf.Encode.Types
  ( EncodingOptions (..),
    setBufferImages,
    setSingleBuffer,
  )
import Options.Applicative
import Types (GltfVariant (..))

data Options = Options
  { exampleName :: String,
    gltfVariant :: GltfVariant,
    prettyPrint :: Bool,
    manyBuffers :: Bool,
    interleaved :: Bool,
    bufferImages :: Bool
  }
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> strOption
      ( long "example"
          <> help "Name of the example"
      )
    <*> flag
      GltfEmbedded
      GltfBinary
      ( long "binary"
          <> help "Output binary format"
      )
    <*> switch
      ( long "pretty-print"
          <> help "Pretty print output JSON"
      )
    <*> switch
      ( long "many-buffers"
          <> help "Create one buffer per mesh"
      )
    <*> switch
      ( long "interleaved"
          <> help "Serialize vertex data in interleaved form"
      )
    <*> switch
      ( long "buffer-images"
          <> help "Store images in buffers"
      )

toEncodingOptions :: Options -> EncodingOptions
toEncodingOptions (Options {gltfVariant, prettyPrint, manyBuffers, interleaved, bufferImages}) =
  def
    { outputVariant = gltfVariant,
      prettyPrint,
      interleaved
    }
    `setSingleBuffer` (not manyBuffers)
    `setBufferImages` bufferImages
