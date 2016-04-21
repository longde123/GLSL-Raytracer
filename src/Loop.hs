{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Loop where

import Control.Concurrent (threadDelay)
import Control.Monad
import System.Clock

toMicroseconds :: TimeSpec -> Int
toMicroseconds time = fromIntegral (toNanoSecs time) `div` 1000

loop :: Int -> s -> (s -> IO (s, Bool)) -> IO ()
loop fps initialState step = do
    time <- getTime Monotonic
    runLoop initialState (time + timePerFrame)
  where
    timePerFrame :: TimeSpec
    timePerFrame = fromNanoSecs $ (10^9) `div` fromIntegral fps

    --runLoop :: s -> TimeSpec -> IO ()
    runLoop state endingSchedule = do
          (newState, shouldStop) <- step state
          nextEndingSchedule <- handleDelay endingSchedule
          unless shouldStop $ runLoop newState nextEndingSchedule

    handleDelay endingSchedule = do
      time <- getTime Monotonic
      let timeLeftToSleep = endingSchedule - time
      if timeLeftToSleep < TimeSpec 0 (-10^8) -- over 100 ms behind schedule
        then do
          --putStrLn $ "Far behind the schedule (" ++ show timeLeftToSleep ++ ")"
          return (time + timePerFrame) -- scrap the schedule, go new one
        else do
          delayWithAccuracy timeLeftToSleep (TimeSpec 0 (5 * 10^5)) -- accuracy of half a millisec
          return (endingSchedule + timePerFrame)

    delayWithAccuracy delayTime accuracy
      | delayTime < accuracy = return ()
      | otherwise = do
        timeBefore <- getTime Monotonic
        threadDelay $ reduce $ toMicroseconds delayTime
        timeAfter <- getTime Monotonic
        let timeSlept = timeAfter - timeBefore
        delayWithAccuracy (delayTime - timeSlept) accuracy

    reduce msTime = msTime - (msTime `div` 10)
