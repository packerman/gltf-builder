module Lib.Assets
  ( getCrate,
    getFloor,
    getGrass,
  )
where

import Data.List (intercalate)
import Gltf.Decode (DataUrl)
import Lib.File (fetchData)
import System.FilePath ((</>))

texturesDir :: FilePath
texturesDir = "downloaded-files" </> "textures"

babylonJsPlaygroundUrl :: String
babylonJsPlaygroundUrl = "https://raw.githubusercontent.com/BabylonJS/Babylon.js/refs/heads/master/packages/tools/playground"

getCrate :: IO DataUrl
getCrate =
  fetchData
    (joinUrl [babylonJsPlaygroundUrl, "public/textures/crate.png"])
    (texturesDir </> "crate.png")

getFloor :: IO DataUrl
getFloor =
  fetchData
    (joinUrl [babylonJsPlaygroundUrl, "public/textures/floor.png"])
    (texturesDir </> "floor.png")

getGrass :: IO DataUrl
getGrass =
  fetchData
    (joinUrl [babylonJsPlaygroundUrl, "public/textures/grass.png"])
    (texturesDir </> "grass.png")

joinUrl :: [String] -> String
joinUrl = intercalate "/"
