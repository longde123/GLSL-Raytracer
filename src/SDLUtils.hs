module SDLUtils where

import qualified SDL
import Data.Text
import Linear
import Foreign
import Foreign.C.Types


runSDL :: IO () -> IO ()
runSDL program = do
    --SDL.initialize [SDL.InitVideo]
    SDL.initializeAll
    program
    SDL.quit

withWindow :: String -> SDL.WindowConfig -> (SDL.Window -> IO ()) -> IO ()
withWindow title config func = do
    window <- SDL.createWindow (pack title) config
    func window
    SDL.destroyWindow window

withRenderer :: SDL.RendererConfig -> SDL.Window -> (SDL.Renderer -> IO ()) -> IO ()
withRenderer rendererConf window func = do
    renderer <- SDL.createRenderer window (-1) rendererConf
    func renderer
    SDL.destroyRenderer renderer

withTexture :: SDL.Renderer -> V2 CInt -> (SDL.Texture -> IO ()) -> IO ()
withTexture rend size action = do
    texture <- SDL.createTexture rend SDL.ARGB8888 SDL.TextureAccessStreaming size
    action texture
    SDL.destroyTexture texture

withPixelsFromTexture :: SDL.Texture -> (Int -> Ptr () -> IO ()) -> IO ()
withPixelsFromTexture texture renderer = do
      (pixels, pitch) <- SDL.lockTexture texture Nothing
      renderer (fromIntegral pitch) pixels
      SDL.unlockTexture texture
