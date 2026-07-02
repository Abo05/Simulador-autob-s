module Motor(runSimulation) where

import Control.Concurrent (threadDelay)
import Passengers (getPassengers, boardPassengers, WaitingPassengers, getPatience)
import Bus (nextBus, Bus(..), dwellTime)
import Events (EventType(..), Event, pushEvent, Time)
import Parser (AppConfig(..))

import System.Random (randomRIO)

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
    
    loop conf tFirstBus 0 firstBusObj finalQueue

loop :: AppConfig -> Time -> WaitingPassengers -> Bus -> [Event] -> IO ()
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
            alightFraction <- randomRIO (0.0, 0.5) :: IO Float
            let currentPass = passengers pendingBus
            let nOut = round (alightFraction * fromIntegral currentPass)
            
            let busAfterAlight = pendingBus { passengers = currentPass - nOut }
            
            let (nIn, newWaiting) = boardPassengers busAfterAlight waiting
            let departingBus = busAfterAlight { passengers = passengers busAfterAlight + nIn }

            let c = tDoors conf
            let timeIn = tIn conf
            let timeOut = tOut conf
            
            dwell <- dwellTime nIn nOut c timeIn timeOut
            
            putStrLn $ cGreen ++ "[" ++ show eventTime ++ "] LLEGA BUS. Bajan: " ++ show nOut ++ 
                       " | Suben: " ++ show nIn ++ " (Dwell: " ++ show dwell ++ ")" ++ cReset
            
            let departureEvent = (eventTime + dwell, BusDeparture)
            let queue = pushEvent departureEvent restQueue
            
            loop conf tNextBus newWaiting departingBus queue

        GroupAbandonment amount -> do

            let totalWaiting = max 0 (waiting - amount)
            putStrLn $ cRed ++ "[" ++ show eventTime ++ "] SE VA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting ++ cReset

            threadDelay 250000 
            loop conf tNextBus totalWaiting pendingBus restQueue

        BusDeparture -> do
            putStrLn $ cGreen ++ "[" ++ show eventTime ++ "] EL BUS SE MARCHA." ++ cReset

            let fr = freq conf
            let cp = cap conf
            (dtNextBus, nextBusObj) <- nextBus fr cp
            
            let absoluteNextBusTime = eventTime + dtNextBus
            let queue1 = pushEvent (absoluteNextBusTime, BusArrival 0) restQueue
            
            let sz = size conf
            let tm = time conf
            let al = sep conf
            groups <- getPassengers sz tm al dtNextBus
            let queue2 = pushPassengers eventTime groups queue1
            
            threadDelay 500000 
            loop conf absoluteNextBusTime waiting nextBusObj queue2
