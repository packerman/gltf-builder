module Gltf.Binary.Chunk
  ( readGlbFile,
    getJsonData,
    getBinaryData,
    GlbFile,
    makeGlbFile,
    writeGlbFile,
  )
where

import Control.Monad.Extra (whenJust)
import Data.Binary
import Data.Binary.Get (bytesRead, getByteString, getWord32le)
import Data.Binary.Put (putByteString, putWord32le)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Header = Header
  { magic :: Word32,
    version :: Word32,
    fileLength :: Word32
  }
  deriving (Eq, Show)

instance Binary Header where
  put (Header {..}) = do
    putWord32le magic
    putWord32le version
    putWord32le fileLength
  get = do
    magic <- getWord32le
    version <- getWord32le
    fileLength <- getWord32le
    return Header {..}

magicValue :: Word32
magicValue = 0x46546C67

versionValue :: Word32
versionValue = 2

makeHeader :: Word32 -> Header
makeHeader fileLength =
  Header
    { fileLength,
      magic = magicValue,
      version = versionValue
    }

data Chunk = Chunk
  { chunkLength :: Word32,
    chunkType :: Word32,
    chunkData :: ByteString
  }
  deriving (Eq, Show)

instance Binary Chunk where
  put (Chunk {..}) = do
    putWord32le chunkLength
    putWord32le chunkType
    putByteString chunkData
  get = do
    chunkLength <- getWord32le
    chunkType <- getWord32le
    chunkData <- getByteString $ fromIntegral chunkLength
    return Chunk {..}

makeChunk :: Word32 -> ByteString -> Chunk
makeChunk chunkType chunkData =
  Chunk
    { chunkLength = fromIntegral $ BS.length chunkData,
      ..
    }

chunkTotalSize :: Chunk -> Word32
chunkTotalSize (Chunk {chunkLength}) = chunkLength + 8

jsonChunkType :: Word32
jsonChunkType = 0x4E4F534A

binChunkType :: Word32
binChunkType = 0x004E4942

data GlbFile = GlbFile
  { header :: Header,
    jsonChunk :: Chunk,
    binaryChunk :: Maybe Chunk
  }
  deriving (Eq, Show)

getJsonData :: GlbFile -> ByteString
getJsonData = chunkData . jsonChunk

getBinaryData :: GlbFile -> Maybe ByteString
getBinaryData = fmap chunkData . binaryChunk

instance Binary GlbFile where
  put (GlbFile {..}) = do
    put header
    put jsonChunk
    whenJust binaryChunk put
  get = do
    header <- get
    jsonChunk <- get
    br <- bytesRead
    binaryChunk <- if br < fromIntegral (fileLength header) then Just <$> get else return Nothing
    return GlbFile {..}

makeGlbFile :: ByteString -> Maybe ByteString -> GlbFile
makeGlbFile jsonData binaryData =
  let jsonChunk = makeChunk jsonChunkType jsonData
      binaryChunk = makeChunk binChunkType <$> binaryData
      header = makeHeader $ chunkTotalSize jsonChunk + maybe 0 chunkTotalSize binaryChunk + 12
   in GlbFile {..}

readGlbFile :: FilePath -> IO GlbFile
readGlbFile filePath = do
  contents <- BSL.readFile filePath
  return $ decode contents

writeGlbFile :: FilePath -> GlbFile -> IO ()
writeGlbFile filePath glbFile =
  BSL.writeFile filePath $ encode glbFile
