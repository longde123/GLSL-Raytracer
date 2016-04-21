module FrameMeasure where

import qualified SDL

import Data.IORef
import Data.StateVar

data FrameMeasure = FrameMeasure { timeOfLastPrint :: Double, framesCounted :: Int }

initFrameMeasure :: IO FrameMeasure
initFrameMeasure = do
  t <- SDL.time
  return $ FrameMeasure t 0

measureFramerate :: IORef FrameMeasure -> IO ()
measureFramerate framerateRef = do
    (FrameMeasure lastPrintTime countedFrames) <- get framerateRef
    time <- SDL.time
    if time - lastPrintTime > 1 -- second passed
      then do
        putStrLn $ "Fps: " ++ show countedFrames ++ " (" ++ show (1000 / fromIntegral countedFrames) ++ " ms)"
        framerateRef $= FrameMeasure time 0
      else
        framerateRef $= FrameMeasure lastPrintTime (countedFrames + 1)
