module DotVox where

import Data.ByteString hiding (map)
import Data.Binary.Get
import Data.Word
import Data.Char

data DotVox = DotVox
  { versionNumber :: Word32
  , mainChunk :: Chunk }
  deriving (Show)

data Chunk = Chunk
  { chunkId :: String
  , chunkSize :: Word32
  , chunkChildrenSize :: Word32
  , chunkContent :: ByteString
  , chunkChildren :: [Chunk] }
  deriving (Show)

sizeOfChunk :: Chunk -> Word32
sizeOfChunk chunk = 4 + 4 + 4 + chunkSize chunk + chunkChildrenSize chunk

getDotVox :: Get DotVox
getDotVox = do
  "VOX " <- getString 4
  versionNumber <- getWord32le
  mainChunk <- getChunk
  return $ DotVox versionNumber mainChunk

getChunk :: Get Chunk
getChunk = do
  chunkId <- getString 4
  chunkSize <- getWord32le
  chunkChildrenSize <- getWord32le
  chunkContent <- getByteString (fromIntegral chunkSize)
  chunkChildren <- isolate (fromIntegral chunkChildrenSize) getChunks
  return $ Chunk chunkId chunkSize chunkChildrenSize chunkContent chunkChildren

getChunks :: Get [Chunk]
getChunks = getEOFTerminatedListOf getChunk

getEOFTerminatedListOf :: Get a -> Get [a]
getEOFTerminatedListOf getIndividual = do
  empty <- isEmpty
  if empty
    then return []
    else do
      individual <- getIndividual
      rest <- getEOFTerminatedListOf getIndividual
      return (individual : rest)

getString :: Int -> Get String
getString size = do
    byteString <- getByteString size
    let word8string = unpack byteString
    return $ map toChar word8string
  where
    toChar word8 = chr $ fromIntegral word8
