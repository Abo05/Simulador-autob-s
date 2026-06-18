module Main (main) where

import Control.Concurrent (threadDelay)
import Passengers (getPassengers, busInStop, WaitingPassengers)
import Bus (nextBus, Bus(..))
import Events (EventType(..), Event, pushEvent)

lambdaGroupSize :: Float
lambdaGroupSize = 1.5   -- Media de personas por grupo (Poisson)

lambdaGroupArrival :: Float
lambdaGroupArrival = 1/2   -- Tasa de llegada de grupos (Exponencial)

lambdaBusArrival :: Float
lambdaBusArrival = 1/5.5  -- Tasa de llegada de autobuses (Exponencial)

main :: IO ()
main = do
    (tFirstGroup, numPassengers) <- getPassengers lambdaGroupSize lambdaGroupArrival
    (tFirstBus, firstBusObj) <- nextBus lambdaBusArrival
    
    let initialQueue = pushEvent (tFirstGroup, GroupArrival numPassengers) []
    let finalQueue = pushEvent (tFirstBus, BusArrival 0) initialQueue
    
    -- Estado inicial: Reloj = 0.0, Pasajeros = 0, Bus Pendiente = firstBusObj, Cola de eventos
    loop 0 (Just firstBusObj) finalQueue

loop :: WaitingPassengers -> Maybe Bus -> [Event] -> IO ()
loop _ _ [] = return ()
loop waiting pendingBus ((eventTime, eventType) : restQueue) = do
    
    case eventType of
        GroupArrival amount -> do
            let totalWaiting = waiting + amount
            putStrLn $ "[" ++ show eventTime ++ "] LLEGA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting
            
            (dt, nextAmount) <- getPassengers lambdaGroupSize lambdaGroupArrival
            let nextEvent = (eventTime + dt, GroupArrival nextAmount)
            let newQueue = pushEvent nextEvent restQueue
            
            threadDelay 250000 
            loop totalWaiting pendingBus newQueue
            
        BusArrival _ -> do
            let arrivingBus = case pendingBus of
                                Just b  -> b
                                Nothing -> Bus 30 0 -- Fallback de seguridad
            
            let leftBehind = busInStop arrivingBus waiting
            let boarded = waiting - leftBehind
            
            putStrLn $ "[" ++ show eventTime ++ "] LLEGA BUS. Han subido: " ++ show boarded ++ " pasajeros. Se quedan: " ++ show leftBehind
            
            (dtNextBus, nextBusObj) <- nextBus lambdaBusArrival
            let nextEvent = (eventTime + dtNextBus, BusArrival 0)
            let newQueue = pushEvent nextEvent restQueue
            
            threadDelay 500000 
            loop leftBehind (Just nextBusObj) newQueue
