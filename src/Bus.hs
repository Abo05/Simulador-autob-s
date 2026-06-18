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
nextBus lambda = do 
                    randTime <- randomRIO(0.000001, 1.0)
                    randPass <- randomRIO(0.0, 1.0) :: IO Float
                    let time = -(log randTime) / lambda
                    let pass = round (fromIntegral busCapacity * randPass)
                    return (time, Bus busCapacity pass)
