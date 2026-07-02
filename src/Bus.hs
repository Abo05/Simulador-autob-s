module Bus (nextBus, Bus(..), dwellTime) where

import System.Random (randomRIO)
import Events (Time)

data Bus = Bus 
    { capacity :: Int,
      passengers :: Int
    } deriving (Show)

nextBus :: Float ->  Int ->  IO (Time, Bus)
nextBus baseTime busCapacity = do 
                    let maxNoise = baseTime * 0.4
                    noise <- randomRIO (-maxNoise, maxNoise)

                    delayChance <- randomRIO (0.0, 1.0) :: IO Float
                    let delay =if delayChance < 0.1
                            then baseTime * 0.6
                            else 0.0

                    let time = baseTime + noise + delay

                    randPass <- randomRIO (0.0, 1.0) :: IO Float
                    let pass = round (fromIntegral busCapacity * randPass)

                    return (time, Bus busCapacity pass)

dwellTime :: Int -> Int -> Float -> Float -> Float -> IO Time
dwellTime nIn nOut c tIn tOut = do
    let dwellIn = c + tIn * fromIntegral nIn
    let dwellOut = c + tOut * fromIntegral nOut

    let baseDwell = max dwellIn dwellOut

    rand <- randomRIO ((-baseDwell/4), baseDwell/2)

    let dwell = rand + baseDwell
    return (max dwell c)

