module Main (main) where

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Passengers (nextPerson, busInStop, WaitingPassengers)
import Bus (nextBus, TimeToNextBus)

main :: IO ()
main = loop 0 0

loop :: WaitingPassengers -> TimeToNextBus -> IO ()
loop passengers time = do
    random <- randomRIO (-1.0, 1.0) :: IO Float
    
    let newPassengers = nextPerson passengers random
    let (newTime, maybeBus) = nextBus random time
    
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
