module Main where

import qualified SDL
import Graphics.Rendering.OpenGL
import Foreign
import Data.Array.Storable hiding ((!))
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Maybe
import Data.IORef
import Linear
import Graphics.GL
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as Repa
import Data.Array.Repa (Z(..), (:.)(..), (!), DIM2, DIM3, U, D)
import Control.Monad

import VoxelLoader
import DotVox
import SDLUtils
import Loop
import FrameMeasure

data State
  = State
  { window :: SDL.Window
  , sdlRenderer :: SDL.Renderer
  , vertexArray :: Ptr Float
  , voxelTex :: TextureObject
  , context :: SDL.GLContext
  , frameMeasure :: IORef FrameMeasure
  }

windowConf :: SDL.WindowConfig
windowConf = SDL.defaultWindow { SDL.windowOpenGL = Just SDL.defaultOpenGL { SDL.glProfile = glProfile } }

glProfile :: SDL.Profile
glProfile = SDL.Compatibility SDL.Normal 3 1

main :: IO ()
main = runSDL $ do
    quadArray <- mkQuadArray
    withWindow "SDL + OpenGL" windowConf $ \window ->
      withRenderer SDL.defaultRenderer window $ \sdlRenderer ->
        withStorableArray quadArray $ \arrayPtr -> do
          SDL.showWindow window
          state <- initializeState arrayPtr window sdlRenderer
          loop 1000 state renderFullscreenQuad
  where
    mkQuadArray = newListArray (0, length quad - 1) quad
    quad = [ -1, -1
           ,  1, -1
           , -1,  1
           ,  1, -1
           ,  1,  1
           , -1,  1 ] :: [Float]
initializeState :: Ptr Float -> SDL.Window -> SDL.Renderer -> IO State
initializeState arrayPtr window sdlRenderer = do
    ctx <- SDL.glCreateContext window

    putStr "loading .vox... "
    -- load voxels from .vox
    dotVox <- runGet getDotVox <$> BS.readFile "chr_sword.vox"
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
    vertSource <- readFile "raytrace_vertex.glsl"
    fragSource <- readFile "raytrace_fragment.glsl"

    program <- loadShaderProgram vertSource fragSource

    currentProgram $= Just program

    -- setup uniforms
    uSize <- get $ uniformLocation program "u_size"
    uVoxels <- get $ uniformLocation program "u_voxels"

    uniform uSize $= Vertex3 (fromIntegral w :: GLint) (fromIntegral h) (fromIntegral d)
    uniform uVoxels $= (0 :: GLint)

    frame <- initFrameMeasure
    frameRef <- newIORef frame
    return State
      { window = window
      , sdlRenderer = sdlRenderer
      , vertexArray = arrayPtr
      , voxelTex = voxelTex
      , context = ctx
      , frameMeasure = frameRef }


renderFullscreenQuad :: State -> IO (State, Bool)
renderFullscreenQuad state = do
    events <- SDL.pollEvents
    let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

    clear [ ColorBuffer ]

    vertexAttribArray (AttribLocation 0) $= Enabled
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, descriptor)

    glActiveTexture GL_TEXTURE0
    textureBinding TextureBuffer' $= Just (voxelTex state)

    drawArrays Triangles 0 6

    vertexAttribArray (AttribLocation 0) $= Disabled

    SDL.glSwapWindow (window state)

    measureFramerate (frameMeasure state)

    return (state, quit)
  where
    descriptor = VertexArrayDescriptor 2 Float (2 * 4) (vertexArray state)

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
    putStrLn $ show shaderType ++ " info log:\n" ++ infoLog
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
