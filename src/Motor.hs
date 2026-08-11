module Motor(runSimulation) where

import Control.Concurrent (threadDelay)
import Passengers (
    Passengers, PassengerGroup(..), totalPassengers,
    pushGroup, popGroup, removeGroup, getPassengers, boardPassengers, getPatience
  )
import Bus (nextBus, Bus(..), dwellTime)
import Events (EventType(..), Event, pushEvent, Time)
import Parser (AppConfig(..))
import System.Random (randomRIO)

cGreen, cYellow, cRed, cReset :: String
cGreen  = "\x1b[32m"
cYellow = "\x1b[33m"
cRed    = "\x1b[31m"
cReset  = "\x1b[0m"

data TransitState = InRoute Bus | InStop Bus

delayDeparture :: Time -> [Event] -> [Event]
delayDeparture _ [] = []
delayDeparture delay ((t, BusDeparture) : xs) = pushEvent (t + delay, BusDeparture) xs
delayDeparture delay (x : xs) = x : delayDeparture delay xs

pushPassengers :: Time -> ([Time], [Int]) -> [Event] -> [Event]
pushPassengers _ ([], _) e = e
pushPassengers _ (_, []) e = e
pushPassengers tm (t : ts, w : ws) e = let event = (tm + t, GroupArrival w)
                                       in pushPassengers tm (ts, ws) (pushEvent event e)

runSimulation :: AppConfig -> IO ()
runSimulation conf = do
    (tFirstBus, firstBusObj) <- nextBus (freq conf) (cap conf) (bNoise conf) (bDelProb conf) (bDelMax conf)
    firstGroups <- getPassengers (size conf) (time conf) (sep conf) tFirstBus
    
    let initialQueue = pushPassengers 0 firstGroups []
    let finalQueue = pushEvent (tFirstBus, BusArrival) initialQueue
    
    loop conf tFirstBus [] (InRoute firstBusObj) finalQueue

loop :: AppConfig -> Time -> Passengers -> TransitState -> [Event] -> IO ()
loop _ _ _ _ [] = return ()
loop conf tNextBus waiting transitState ((eventTime, eventType) : restQueue) = do
    
    case eventType of
        GroupArrival amount -> do
            let penalization = penFull conf
            (nBoarded, newWaitingList, finalState, queueWithDelay, penalty) <- case transitState of
                InStop parkedBus -> do
                    let availableSpace = capacity parkedBus - passengers parkedBus
                    if amount <= availableSpace
                        then do
                            let updatedBus = parkedBus { passengers = passengers parkedBus + amount }
                            let delay = tIn conf * fromIntegral amount
                            putStrLn $ cGreen ++ " -> El grupo de " ++ show amount ++ " cabe entero y sube corriendo. Retrasan la salida " ++ show delay ++ cReset
                            return (amount, waiting, InStop updatedBus, delayDeparture delay restQueue, 1.0)
                        else do
                            let newGroup = PassengerGroup { arrivalTime = eventTime, groupSize = amount, patience = 0 }
                            putStrLn $ cRed ++ " -> Llegan corriendo pero el grupo (" ++ show amount ++ ") no cabe en las plazas libres (" ++ show availableSpace ++ "). Frustración aumentada." ++ cReset
                            return (0, pushGroup newGroup waiting, transitState, restQueue, penalization)
                InRoute _ -> do
                    let newGroup = PassengerGroup { arrivalTime = eventTime, groupSize = amount, patience = 0 }
                    return (0, pushGroup newGroup waiting, transitState, restQueue, 1.0)

            let totalWaitingCount = totalPassengers newWaitingList
            putStrLn $ cYellow ++ "[" ++ show eventTime ++ "] LLEGA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaitingCount ++ cReset

            let timeNextBus = case finalState of
                                InStop _  -> freq conf
                                InRoute _ -> max 0 (tNextBus - eventTime)
            
            -- Evaluación de paciencia solo si el nuevo grupo (o parte de él) se quedó en la parada
            let lastGroupLeftOver = amount - nBoarded
            queue <- if lastGroupLeftOver > 0
                then do
                    maybePatience <- getPatience (patPass conf * penalty) (patTime conf * penalty) totalWaitingCount timeNextBus
                    case maybePatience of
                        Just waitingTime -> return $ pushEvent (eventTime + waitingTime, GroupAbandonment eventTime) queueWithDelay
                        Nothing -> return queueWithDelay
                else return queueWithDelay

            threadDelay (simDelay conf) 
            loop conf tNextBus newWaitingList finalState queue
            
        BusArrival -> do
            let incomingBus = case transitState of
                                InRoute b -> b
                                InStop b -> b
            
            alightFraction <- randomRIO (0.0, alightMax conf) :: IO Float
            let currentPass = passengers incomingBus
            let nOut = round (alightFraction * fromIntegral currentPass)
            let busAfterAlight = incomingBus { passengers = currentPass - nOut }
            let availableSpace = capacity busAfterAlight - passengers busAfterAlight
            
            let (nIn, newWaitingList) = boardPassengers busAfterAlight waiting
            let departingBus = busAfterAlight { passengers = passengers busAfterAlight + nIn }
            
            dwell <- dwellTime nIn nOut (tDoors conf) (tIn conf) (tOut conf) (dwNMin conf) (dwNMax conf)
            
            putStrLn $ cGreen ++ "[" ++ show eventTime ++ "] LLEGA BUS. Bajan: " ++ show nOut ++ 
                       " | Plazas libres: " ++ show availableSpace ++
                       " | Suben: " ++ show nIn ++ " (Dwell: " ++ show dwell ++ ")" ++ cReset
            
            let departureEvent = (eventTime + dwell, BusDeparture)
            let queue1 = pushEvent departureEvent restQueue
            
            groupsDwell <- getPassengers (size conf) (time conf) (sep conf) dwell
            let finalQueue = pushPassengers eventTime groupsDwell queue1

            let penalization = penFull conf
            let totalWaitingCount = totalPassengers newWaitingList
            queuePenalize <- if totalWaitingCount > 0
                then do
                    putStrLn $ cRed ++ "[" ++ show eventTime ++ "] Bus lleno/parcial. " ++ show totalWaitingCount ++ " personas se quedan en tierra y evalúan marcharse." ++ cReset
                    let timeNextBus = freq conf
                    maybePatience <- getPatience (patPass conf * penalization) (patTime conf * penalization) totalWaitingCount timeNextBus
                    case maybePatience of
                        Just waitingTime -> case popGroup newWaitingList of
                            Just (headGroup, _) -> return $ pushEvent (eventTime + waitingTime, GroupAbandonment (arrivalTime headGroup)) finalQueue
                            Nothing             -> return finalQueue
                        Nothing -> return finalQueue
                else return finalQueue
            
            loop conf tNextBus newWaitingList (InStop departingBus) queuePenalize

        GroupAbandonment tArr -> do
            case removeGroup tArr waiting of
                (Just targetGroup, newWaitingList) -> do
                    let amountLeft = groupSize targetGroup
                    let totalWaitingCount = totalPassengers newWaitingList
                    putStrLn $ cRed ++ "[" ++ show eventTime ++ "] SE VA GRUPO. Tamaño: " ++ show amountLeft ++ ". Esperando en parada: " ++ show totalWaitingCount ++ cReset
                    threadDelay (simDelay conf) 
                    loop conf tNextBus newWaitingList transitState restQueue
                (Nothing, _) -> do
                    -- El grupo ya había embarcado en el autobús antes de la expiración de la paciencia
                    loop conf tNextBus waiting transitState restQueue

        BusDeparture -> do
            putStrLn $ cGreen ++ "[" ++ show eventTime ++ "] EL BUS SE MARCHA." ++ cReset

            let fr = freq conf
            let cp = cap conf
            (dtNextBus, nextBusObj) <- nextBus fr cp (bNoise conf) (bDelProb conf) (bDelMax conf)
            
            let absoluteNextBusTime = eventTime + dtNextBus
            let queue1 = pushEvent (absoluteNextBusTime, BusArrival) restQueue
            
            groups <- getPassengers (size conf) (time conf) (sep conf) dtNextBus
            let queue2 = pushPassengers eventTime groups queue1
            
            threadDelay (simDelay conf * 2) 
            loop conf absoluteNextBusTime waiting (InRoute nextBusObj) queue2
