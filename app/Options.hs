module Options (module Options) where

import Gltf.Encode.Types (BufferCreate (..), EncodingOptions (..))
import Options.Applicative

data Options = Options
  { exampleName :: String,
    prettyPrint :: Bool,
    bufferCreate :: BufferCreate
  }
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> strOption
      ( long "example"
          <> help "Name of the example"
      )
    <*> switch
      ( long "pretty-print"
          <> help "Pretty print output JSON"
      )
    <*> flag
      SingleBuffer
      OnePerMesh
      ( long "buffer-per-mesh"
          <> help "Create one buffer per mesh"
      )

toEncodingOptions :: Options -> EncodingOptions
toEncodingOptions (Options {prettyPrint, bufferCreate}) =
  EncodingOptions {prettyPrint, bufferCreate}
