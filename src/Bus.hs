module Bus (nextBus, Bus(..), dwellTime) where

import System.Random (randomRIO)
import Events (Time)

data Bus = Bus 
    { capacity :: Int,
      passengers :: Int
    } deriving (Show)

nextBus :: Float -> Int -> Float -> Float -> Float -> IO (Time, Bus)
nextBus baseTime busCapacity busNoiseMax busDelayProb busDelayMax = do 
    let maxNoise = baseTime * busNoiseMax
    noise <- randomRIO (-maxNoise, maxNoise)

    delayChance <- randomRIO (0.0, 1.0) :: IO Float
    let delay = if delayChance < busDelayProb
                then baseTime * busDelayMax
                else 0.0

    let time = baseTime + noise + delay

    randPass <- randomRIO (0.0, 1.0) :: IO Float
    let pass = round (fromIntegral busCapacity * randPass)

    return (time, Bus busCapacity pass)

dwellTime :: Int -> Int -> Float -> Float -> Float -> Float -> Float -> IO Time
dwellTime 0 0 _ _ _ _ _ = return 0
dwellTime nIn nOut c tIn tOut dwNMin dwNMax = do
    let dwellIn = c + tIn * fromIntegral nIn
    let dwellOut = c + tOut * fromIntegral nOut

    let baseDwell = max dwellIn dwellOut

    rand <- randomRIO (baseDwell * dwNMin, baseDwell * dwNMax)

    let dwell = rand + baseDwell
    return (max dwell c)
