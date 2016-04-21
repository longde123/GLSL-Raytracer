module VoxelLoader where

import Data.ByteString hiding (map, filter)
import qualified Data.ByteString.Lazy as LazyBS
import Data.Binary.Get
import Data.Word
import Data.Char
import Data.Maybe
import Data.Array.Repa as Repa
import Data.Bits
import Linear

import DotVox

getSize :: Get DIM3
getSize = do
  w <- getWord32le
  h <- getWord32le
  d <- getWord32le
  return $ Z :. fromIntegral w :. fromIntegral h :. fromIntegral d

getVoxels :: Get [(DIM3, Word8)]
getVoxels = do
    skip 4
    getEOFTerminatedListOf getVoxel
  where
    getVoxel = do
      x <- getWord8
      y <- getWord8
      z <- getWord8
      colIdx <- getWord8
      return (Z :. fromIntegral x :. fromIntegral z :. fromIntegral y, colIdx)

getColorPalette :: Get [Word32]
getColorPalette = getEOFTerminatedListOf getColor

getColor :: Get Word32
getColor = do
  r <- getWord8
  g <- getWord8
  b <- getWord8
  a <- getWord8
  return $ shiftL (fromIntegral a) 24
         + shiftL (fromIntegral r) 16
         + shiftL (fromIntegral g) 8
         +         fromIntegral b

findChunkWithId :: String -> [Chunk] -> Maybe Chunk
findChunkWithId identifier chunks =
  listToMaybe $ filter (\chunk -> chunkId chunk == identifier) chunks

voxelsFromDotVox :: DotVox -> Maybe (Repa.Array U DIM3 Word32)
voxelsFromDotVox dotVox = do
  sizeChunk <- findChunkWithId "SIZE" (chunkChildren $ mainChunk dotVox)
  voxelChunk <- findChunkWithId "XYZI" (chunkChildren $ mainChunk dotVox)
  paletteChunk <- findChunkWithId "RGBA" (chunkChildren $ mainChunk dotVox)
  let size = runGet getSize $ LazyBS.fromStrict $ chunkContent sizeChunk
  let voxels = runGet getVoxels $ LazyBS.fromStrict $ chunkContent voxelChunk
  let palette = runGet getColorPalette $ LazyBS.fromStrict $ chunkContent paletteChunk
  return $ computeS $ fromFunction size (findVoxel voxels palette)

findVoxel :: [(DIM3, Word8)] -> [Word32] -> DIM3 -> Word32
findVoxel voxels palette pos =
  case lookup pos voxels of
    Just 0 -> 0
    Just colorIndex -> palette !! fromIntegral (colorIndex - 1)
    Nothing -> 0
