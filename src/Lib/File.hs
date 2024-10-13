module Lib.File
  ( downloadFile,
    writeNestedFile,
    fetchData,
  )
where

import Control.Lens
import Control.Monad.Extra (ifM)
import Data.ByteString.Lazy as BSL
import Data.String (fromString)
import Lib.Base64 (DataUrl, dataUrl)
import Network.Mime (defaultMimeLookup)
import Network.Wreq as HTTP
import System.Directory
import System.FilePath

fetchData :: String -> FilePath -> IO DataUrl
fetchData url filePath =
  let mimetype = defaultMimeLookup $ fromString $ takeFileName filePath
   in dataUrl mimetype . BSL.toStrict <$> downloadFile url filePath

downloadFile :: String -> FilePath -> IO BSL.ByteString
downloadFile url filePath =
  ifM (doesFileExist filePath) (BSL.readFile filePath) downloadFile'
  where
    downloadFile' = do
      reponse <- HTTP.get url
      let body = reponse ^. responseBody
      createDirectoryForFile filePath
      BSL.writeFile filePath body
      return body

writeNestedFile :: FilePath -> BSL.ByteString -> IO ()
writeNestedFile filePath content = do
  createDirectoryForFile filePath
  BSL.writeFile filePath content

createDirectoryForFile :: FilePath -> IO ()
createDirectoryForFile filePath = do
  let directoryPath = takeDirectory filePath
  createDirectoryIfMissing True directoryPath
