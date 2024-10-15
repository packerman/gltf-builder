module Main (main) where

import qualified Data.Map as M
import Example
import Example.Square
import Example.TexturedBox
import Lib.Base (maybeToM)

examples :: M.Map String (IO Example)
examples =
  M.fromList
    [ ("square", pure Example.Square.example),
      ("textured-box", Example.TexturedBox.example)
    ]

main :: IO ()
main =
  let name = "textured-box"
   in sequence (M.lookup name examples)
        >>= maybeToM (unwords ["Example", name, "not found"])
        >>= runExample
