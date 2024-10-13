module Main (main) where

import Example (runExample)
import Example.Square

main :: IO ()
main = runExample Example.Square.example
