module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Foreign
import Data.IORef
import Data.Array.Storable hiding ((!))
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Maybe
import Linear
import Graphics.GL
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as Repa
import Data.Array.Repa (Z(..), (:.)(..), (!), DIM2, DIM3, U, D)
import Control.Monad
import Data.Time.Clock.POSIX

import VoxelLoader
import DotVox

main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    window <- createWindow "Hello World"

    putStr "loading .vox... "
    -- load voxels from .vox
    dotVox <- runGet getDotVox <$> BS.readFile "dragon.vox"
    let voxels = fromJust $ voxelsFromDotVox dotVox
    let (Z :. w :. h :. d) = Repa.extent voxels
    putStrLn "done."

    putStr "Setting up TBO... "
    -- setup texture buffer object with voxel data
    -- first the buffer object
    bufferObjectId <- glGenBuffer
    glBindBuffer GL_TEXTURE_BUFFER bufferObjectId

    withRepaArray voxels $ \voxelsPtr -> do
      let voxelsSize = w * h * d
      glBufferData GL_TEXTURE_BUFFER (fromIntegral voxelsSize * 4) voxelsPtr GL_STATIC_DRAW

    -- next the texture object
    voxelTex <- genObjectName :: IO TextureObject
    textureBinding TextureBuffer' $= Just voxelTex
    glTexBuffer GL_TEXTURE_BUFFER GL_R8UI bufferObjectId
    putStrLn "done."

    -- setup shader
    vertSource <- readFile "vertex.glsl"
    fragSource <- readFile "fragment.glsl"

    program <- loadShaderProgram vertSource fragSource

    currentProgram $= Just program

    -- setup uniforms
    uSize <- get $ uniformLocation program "u_size"
    uVoxels <- get $ uniformLocation program "u_voxels"

    uniform uSize $= Vertex3 (fromIntegral w :: GLint) (fromIntegral h) (fromIntegral d)
    uniform uVoxels $= (0 :: GLint)

    -- render with vertex array object
    quadArray <- mkQuadArray
    time <- round . (* 1000) <$> getPOSIXTime
    lastSecondRef <- newIORef time
    framesCountRef <- newIORef 0
    withStorableArray quadArray $ \arrayPtr -> do
      displayCallback $= renderFullscreenQuad lastSecondRef framesCountRef arrayPtr voxelTex
      mainLoop
  where
    mkQuadArray = newListArray (0, length quad - 1) quad
    quad = [ -1, -1
           ,  1, -1
           , -1,  1
           ,  1, -1
           ,  1,  1
           , -1,  1 ] :: [Float]

renderFullscreenQuad :: IORef Int -> IORef Int -> Ptr Float -> TextureObject -> IO ()
renderFullscreenQuad lastSecondRef framesCountRef arrayPtr voxelTex = do
    clear [ ColorBuffer ]

    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, descriptor)

    glActiveTexture GL_TEXTURE0
    textureBinding TextureBuffer' $= Just voxelTex

    drawArrays Triangles 0 6

    vertexAttribArray (AttribLocation 0) $= Disabled

    flush

    maybePrintFPS lastSecondRef framesCountRef

    postRedisplay Nothing
  where
    descriptor = VertexArrayDescriptor 2 Float (2 * 4) arrayPtr

maybePrintFPS :: IORef Int -> IORef Int -> IO ()
maybePrintFPS lastSecondRef framesCountRef = do
    modifyIORef framesCountRef (+ 1)
    time <- round . (* 1000) <$> getPOSIXTime
    lastSecond <- readIORef lastSecondRef
    let sinceLastSecond = time - lastSecond
    when (sinceLastSecond >= 1000) $ do
      frames <- readIORef framesCountRef
      putStrLn $ "Fps: " ++ show frames ++ " (" ++ show (1000.0 / fromIntegral frames) ++ " ms)"
      writeIORef framesCountRef 0
      writeIORef lastSecondRef time

loadShaderProgram :: String -> String -> IO Program
loadShaderProgram vertSource fragSource = do
    vertexShader <- loadShaderObject VertexShader vertSource
    fragmentShader <- loadShaderObject FragmentShader fragSource
    program <- createProgram
    attachShader program vertexShader
    attachShader program fragmentShader
    attribLocation program "position" $= AttribLocation 0
    linkProgram program
    infoLog <- get $ programInfoLog program
    putStrLn $ "Shader Program info log:\n" ++ infoLog
    return program

loadShaderObject :: ShaderType -> String -> IO Shader
loadShaderObject shaderType source = do
    shader <- createShader shaderType
    shaderSourceBS shader $= packUtf8 source
    compileShader shader
    infoLog <- get $ shaderInfoLog shader
    return shader

glGenBuffer :: IO GLuint
glGenBuffer =
  allocaArray 1 $ \buf -> do
    glGenBuffers 1 buf
    head <$> peekArray 1 buf

withRepaArray :: Repa.Array U DIM3 Word32 -> (Ptr Word32 -> IO ()) -> IO ()
withRepaArray array acceptor =
    allocaArray size $ \ptr -> do
      idx <- newIORef 0
      forM_ [0..d-1] $ \z ->
        forM_ [0..h-1] $ \y ->
          forM_ [0..w-1] $ \x -> do
            index <- readIORef idx
            modifyIORef idx (+ 1)
            pokeElemOff ptr index (array ! (Z :. x :. y :. z))
      acceptor ptr
  where
    size = w * h * d
    (Z :. w :. h :. d) = Repa.extent array
