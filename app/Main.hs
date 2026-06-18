module Main (main) where

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Passengers (updatePassengers, busInStop, WaitingPassengers)
import Bus (nextBus, TimeToNextBus)

lambda :: Float
lambda = 1.5

main :: IO ()
main = loop 0 0

loop :: WaitingPassengers -> TimeToNextBus -> IO ()
loop passengers time = do
    randomBus <- randomRIO (-1.0, 1.0) :: IO Float
    
    newPassengers <- updatePassengers passengers lambda
    let (newTime, maybeBus) = nextBus randomBus time
    
    nextIterationPassengers <- case maybeBus of
        Nothing -> do
            putStrLn $ "Esperando... (Tiempo restante: " ++ show newTime ++ ") | Pasajeros en parada: " ++ show newPassengers
            return newPassengers
            
        Just bus -> do
            let leftBehind = busInStop bus newPassengers
            let boarded = newPassengers - leftBehind
            putStrLn $ "¡LLEGA EL BUS! Han subido " ++ show boarded ++ " pasajeros. Se quedan " ++ show leftBehind ++ " esperando."
            return leftBehind
    
    threadDelay 500000
    loop nextIterationPassengers newTime
