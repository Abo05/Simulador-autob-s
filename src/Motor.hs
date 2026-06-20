module Motor(runSimulation) where

import Control.Concurrent (threadDelay)
import Passengers (getPassengers, busInStop, WaitingPassengers)
import Bus (nextBus, Bus(..))
import Events (EventType(..), Event, pushEvent, Time)
import Parser (AppConfig(..))

pushPassengers :: Time -> ([Time], [WaitingPassengers]) -> [Event] -> [Event]
pushPassengers _ ([], _) e = e
pushPassengers _ (_, []) e = e
pushPassengers tm (t : ts, w : ws) e = if t <= 0
                                       then pushPassengers tm (ts, ws) e
                                       else let event = (tm + t, GroupArrival w)
                                            in pushPassengers tm (ts, ws) (pushEvent event e)


runSimulation :: AppConfig -> IO ()
runSimulation conf = do
    (tFirstBus, firstBusObj) <- nextBus (freq conf) (cap conf)
    firstGroups <- getPassengers (size conf) (time conf) (sep conf) tFirstBus
    
    let initialQueue = pushPassengers 0 firstGroups []
    let finalQueue = pushEvent (tFirstBus, BusArrival 0) initialQueue
    
    loop conf 0 (Just firstBusObj) finalQueue

loop :: AppConfig -> WaitingPassengers -> Maybe Bus -> [Event] -> IO ()
loop _ _ _ [] = return ()
loop conf waiting pendingBus ((eventTime, eventType) : restQueue) = do
    
    case eventType of
        GroupArrival amount -> do
            let totalWaiting = waiting + amount
            putStrLn $ "[" ++ show eventTime ++ "] LLEGA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting
            
            
            threadDelay 250000 
            loop conf totalWaiting pendingBus restQueue
            
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
            let queue = pushEvent nextEvent restQueue
            
            let sz = size conf
            let tm = time conf
            let al = sep conf
            groups <- getPassengers sz tm al dtNextBus
            let newQueue = pushPassengers eventTime groups queue
            threadDelay 500000 
            loop conf leftBehind (Just nextBusObj) newQueue
