module Assets
  ( getCrate,
    getFloor,
    getGrass,
  )
where

import Gltf.Decode (DataUrl)
import Lib.File (fetchData)
import Lib.URI (relativeToM)
import System.FilePath ((</>))

texturesDir :: FilePath
texturesDir = "downloaded-files" </> "textures"

babylonJsPlaygroundUrl :: String
babylonJsPlaygroundUrl = "https://raw.githubusercontent.com/BabylonJS/Babylon.js/refs/heads/master/packages/tools/playground/"

getCrate :: IO DataUrl
getCrate = fetchAsset "public/textures/crate.png" babylonJsPlaygroundUrl (texturesDir </> "crate.png")

getFloor :: IO DataUrl
getFloor = fetchAsset "public/textures/floor.png" babylonJsPlaygroundUrl (texturesDir </> "floor.png")

getGrass :: IO DataUrl
getGrass = fetchAsset "public/textures/grass.png" babylonJsPlaygroundUrl (texturesDir </> "grass.png")

fetchAsset :: String -> String -> FilePath -> IO DataUrl
fetchAsset relativeUri referenceUri filePath =
  relativeToM relativeUri referenceUri >>= flip fetchData filePath
