module Lib.File (downloadFile) where

import Control.Lens
import Data.ByteString.Lazy as BSL
import Network.Wreq as HTTP
import System.Directory
import System.FilePath

downloadFile :: String -> FilePath -> IO BSL.ByteString
downloadFile url filePath = do
  fileExists <- doesFileExist filePath
  if fileExists then BSL.readFile filePath else downloadFile'
  where
    downloadFile' = do
      reponse <- HTTP.get url
      let body = reponse ^. responseBody
      createDirectoryForFile filePath
      BSL.writeFile filePath body
      return body

createDirectoryForFile :: FilePath -> IO ()
createDirectoryForFile filePath = do
  let directoryPath = takeDirectory filePath
  createDirectoryIfMissing True directoryPath
