module Motor(runSimulation) where

import Control.Concurrent (threadDelay)
import Passengers (getPassengers, busInStop, WaitingPassengers)
import Bus (nextBus, Bus(..))
import Events (EventType(..), Event, pushEvent)
import Parser (AppConfig(..))

runSimulation :: AppConfig -> IO ()
runSimulation conf = do
    (tFirstGroup, numPassengers) <- getPassengers (size conf) (time conf)
    (tFirstBus, firstBusObj) <- nextBus (freq conf) (cap conf)
    
    let initialQueue = pushEvent (tFirstGroup, GroupArrival numPassengers) []
    let finalQueue = pushEvent (tFirstBus, BusArrival 0) initialQueue
    
    loop conf 0 (Just firstBusObj) finalQueue

loop :: AppConfig -> WaitingPassengers -> Maybe Bus -> [Event] -> IO ()
loop _ _ _ [] = return ()
loop conf waiting pendingBus ((eventTime, eventType) : restQueue) = do
    
    case eventType of
        GroupArrival amount -> do
            let totalWaiting = waiting + amount
            putStrLn $ "[" ++ show eventTime ++ "] LLEGA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting
            
            let sz = size conf
            let tm = time conf
            (dt, nextAmount) <- getPassengers sz tm
            let nextEvent = (eventTime + dt, GroupArrival nextAmount)
            let newQueue = pushEvent nextEvent restQueue
            
            threadDelay 250000 
            loop conf totalWaiting pendingBus newQueue
            
        BusArrival _ -> do
            let arrivingBus = case pendingBus of
                                Just b  -> b
                                Nothing -> Bus (cap conf) 0
            
            let leftBehind = busInStop arrivingBus waiting
            let boarded = waiting - leftBehind
            
            putStrLn $ "[" ++ show eventTime ++ "] LLEGA BUS. Han subido: " ++ show boarded ++ " pasajeros. Se quedan: " ++ show leftBehind
            
            let fr = freq conf
            let cp = cap conf
            (dtNextBus, nextBusObj) <- nextBus fr cp
            let nextEvent = (eventTime + dtNextBus, BusArrival 0)
            let newQueue = pushEvent nextEvent restQueue
            
            threadDelay 500000 
            loop conf leftBehind (Just nextBusObj) newQueue
