module Motor(runSimulation) where

import Control.Concurrent (threadDelay)
import Passengers (getPassengers, busInStop, WaitingPassengers, getPatience)
import Bus (nextBus, Bus(..))
import Events (EventType(..), Event, pushEvent, Time)
import Parser (AppConfig(..))

cGreen, cYellow, cRed, cReset :: String
cGreen  = "\x1b[32m"
cYellow = "\x1b[33m"
cRed    = "\x1b[31m"
cReset  = "\x1b[0m"

pushPassengers :: Time -> ([Time], [WaitingPassengers]) -> [Event] -> [Event]
pushPassengers _ ([], _) e = e
pushPassengers _ (_, []) e = e
pushPassengers tm (t : ts, w : ws) e = let event = (tm + t, GroupArrival w)
                                       in pushPassengers tm (ts, ws) (pushEvent event e)


runSimulation :: AppConfig -> IO ()
runSimulation conf = do
    (tFirstBus, firstBusObj) <- nextBus (freq conf) (cap conf)
    firstGroups <- getPassengers (size conf) (time conf) (sep conf) tFirstBus
    
    let initialQueue = pushPassengers 0 firstGroups []
    let finalQueue = pushEvent (tFirstBus, BusArrival 0) initialQueue
    
    loop conf tFirstBus 0 (Just firstBusObj) finalQueue

loop :: AppConfig -> Time -> WaitingPassengers -> Maybe Bus -> [Event] -> IO ()
loop _ _ _ _ [] = return ()
loop conf tNextBus waiting pendingBus ((eventTime, eventType) : restQueue) = do
    
    case eventType of
        GroupArrival amount -> do
            let totalWaiting = waiting + amount
            putStrLn $ cYellow ++ "[" ++ show eventTime ++ "] LLEGA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting ++ cReset
            
            let timeNextBus = max 0 (tNextBus - eventTime)
            let l1 = patPass conf
            let l2 = patTime conf
            patience <- getPatience l1 l2 totalWaiting timeNextBus

            let queue = case patience of
                            Just waitingTime -> 
                                let absoluteAbandonTime = eventTime + waitingTime
                                    event = (absoluteAbandonTime, GroupAbandonment amount)
                                in pushEvent event restQueue
                            Nothing -> restQueue
            
            threadDelay 250000 
            loop conf tNextBus totalWaiting pendingBus queue
            
        BusArrival _ -> do
            let arrivingBus = case pendingBus of
                                Just b  -> b
                                Nothing -> Bus (cap conf) 0
            
            let leftBehind = busInStop arrivingBus waiting
            let boarded = waiting - leftBehind
            
            putStrLn $ cGreen ++ "[" ++ show eventTime ++ "] LLEGA BUS. Han subido: " ++ show boarded ++ " pasajeros. Se quedan: " ++ show leftBehind ++ cReset
            
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
            let absoluteNextBusTime = eventTime + dtNextBus

            threadDelay 500000 

            loop conf absoluteNextBusTime leftBehind (Just nextBusObj) newQueue

        GroupAbandonment amount -> do

            let totalWaiting = max 0 (waiting - amount)
            putStrLn $ cRed ++ "[" ++ show eventTime ++ "] SE VA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting ++ cReset

            threadDelay 250000 
            loop conf tNextBus totalWaiting pendingBus restQueue
