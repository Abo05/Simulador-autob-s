module Bus (nextBus, Bus(..)) where

import System.Random (randomRIO)
import Events (Time)

busCapacity :: Int
busCapacity = 30

data Bus = Bus 
    { capacity :: Int,
      passengers :: Int
    } deriving (Show)

nextBus :: Float ->  IO (Time, Bus)
nextBus baseTime = do 
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
