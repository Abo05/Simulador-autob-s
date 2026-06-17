module Main (main) where

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Passengers (nextPerson, busInStop)
import Bus (nextBus)

main :: IO ()
main = loop 0

loop :: Int -> IO ()
loop passengers = do
    random <- randomRIO (0.0, 5.0) :: IO Float
    
    let newPassengers = nextPerson passengers random
    
    let resultado = case nextBus random of
                        Nothing -> newPassengers
                        Just bus -> busInStop bus newPassengers
    
    print resultado
    
    threadDelay 500000
    loop resultado
