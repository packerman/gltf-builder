module Main (main) where

import qualified Data.Map as M
import Example
import Example.Square
import Example.TexturedBox
import Lib.Base (maybeToM)
import Options.Applicative

examples :: M.Map String (IO Example)
examples =
  M.fromList
    [ ("square", pure Example.Square.example),
      ("textured-box", Example.TexturedBox.example)
    ]

newtype Options = Options
  { exampleName :: String
  }
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> strOption
      ( long "example"
          <> help "Name of the example"
      )

main :: IO ()
main =
  mainExample =<< execParser parserInfo
  where
    mainExample
      ( Options
          { exampleName
          }
        ) =
        sequence (M.lookup exampleName examples)
          >>= checkIfFound exampleName
          >>= runExample

    checkIfFound exampleName =
      maybeToM $
        unwords ["Example", exampleName, "not found. Available examples:", show $ M.keys examples]

    parserInfo = info parseOptions fullDesc
