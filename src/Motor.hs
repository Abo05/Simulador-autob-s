module Motor(runSimulation) where

import Control.Concurrent (threadDelay)
import Passengers (getPassengers, boardPassengers, WaitingPassengers(..), getPatience)
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

pushPassengers :: Time -> ([Time], [WaitingPassengers]) -> [Event] -> [Event]
pushPassengers _ ([], _) e = e
pushPassengers _ (_, []) e = e
pushPassengers tm (t : ts, w : ws) e = let event = (tm + t, GroupArrival (getCount w))
                                       in pushPassengers tm (ts, ws) (pushEvent event e)


runSimulation :: AppConfig -> IO ()
runSimulation conf = do
    (tFirstBus, firstBusObj) <- nextBus (freq conf) (cap conf) (bNoise conf) (bDelProb conf) (bDelMax conf)
    firstGroups <- getPassengers (size conf) (time conf) (sep conf) tFirstBus
    
    let initialQueue = pushPassengers 0 firstGroups []
    let finalQueue = pushEvent (tFirstBus, BusArrival) initialQueue
    
    loop conf tFirstBus (WaitingPassengers 0) (InRoute firstBusObj) finalQueue

loop :: AppConfig -> Time -> WaitingPassengers -> TransitState -> [Event] -> IO ()
loop _ _ _ _ [] = return ()
loop conf tNextBus waiting transitState ((eventTime, eventType) : restQueue) = do
    
    case eventType of
        GroupArrival amount -> do
            let totalWaiting = WaitingPassengers (getCount waiting + amount)
            putStrLn $ cYellow ++ "[" ++ show eventTime ++ "] LLEGA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting ++ cReset
            
            -- Lógica de embarque dinámico si el bus está estacionado
            let penalization = penFull conf
            (finalWaiting, finalState, queueWithDelay, penalty) <- case transitState of
                InStop parkedBus -> do
                    let (nIn, leftBehind) = boardPassengers parkedBus totalWaiting
                    if nIn > 0
                        then do
                            let updatedBus = parkedBus { passengers = passengers parkedBus + nIn }
                            let delay = tIn conf * fromIntegral nIn
                            putStrLn $ cGreen ++ " -> " ++ show nIn ++ " logran subir corriendo. Retrasan la salida " ++ show delay ++ cReset
                            return (leftBehind, InStop updatedBus, delayDeparture delay restQueue, 1.0)
                        else do
                            putStrLn $ cRed ++ " -> Llegan corriendo pero el bus está lleno. Frustración aumentada." ++ cReset
                            return (totalWaiting, transitState, restQueue, penalization)
                InRoute _ -> return (totalWaiting, transitState, restQueue, 1.0)

            -- Evaluación de paciencia sobre los que no han logrado subir
            let timeNextBus = case transitState of
                                InStop _  -> freq conf
                                InRoute _ -> max 0 (tNextBus - eventTime)
            patience <- getPatience (patPass conf * penalty) (patTime conf * penalty) finalWaiting timeNextBus
            
            let queue = case patience of
                            Just waitingTime -> pushEvent (eventTime + waitingTime, GroupAbandonment amount) queueWithDelay
                            Nothing -> queueWithDelay
            
            threadDelay (simDelay conf) 
            loop conf tNextBus finalWaiting finalState queue
            
        BusArrival -> do
            let incomingBus = case transitState of
                                InRoute b -> b
                                InStop b -> b
            
            alightFraction <- randomRIO (0.0, alightMax conf) :: IO Float
            let currentPass = passengers incomingBus
            let nOut = round (alightFraction * fromIntegral currentPass)
            let busAfterAlight = incomingBus { passengers = currentPass - nOut }
            
            let (nIn, newWaiting) = boardPassengers busAfterAlight waiting
            let departingBus = busAfterAlight { passengers = passengers busAfterAlight + nIn }
            
            dwell <- dwellTime nIn nOut (tDoors conf) (tIn conf) (tOut conf) (dwNMin conf) (dwNMax conf)
            
            putStrLn $ cGreen ++ "[" ++ show eventTime ++ "] LLEGA BUS. Bajan: " ++ show nOut ++ 
                       " | Suben: " ++ show nIn ++ " (Dwell: " ++ show dwell ++ ")" ++ cReset
            
            let departureEvent = (eventTime + dwell, BusDeparture)
            let queue1 = pushEvent departureEvent restQueue
            
            -- Generación de pasajeros durante la ventana de puertas abiertas
            groupsDwell <- getPassengers (size conf) (time conf) (sep conf) dwell
            let finalQueue = pushPassengers eventTime groupsDwell queue1

            let penalization = penFull conf
            queuePenalize <- if getCount newWaiting > 0
                then do
                    putStrLn $ cRed ++ "[" ++ show eventTime ++ "] Bus lleno. " ++ show newWaiting ++ " personas se quedan en tierra y evalúan marcharse." ++ cReset
                    let timeNextBus = freq conf
                    patience <- getPatience (patPass conf * penalization) (patTime conf * penalization) newWaiting timeNextBus
                    case patience of
                        Just waitingTime -> return $ pushEvent (eventTime + waitingTime, GroupAbandonment (getCount newWaiting)) finalQueue
                        Nothing -> return finalQueue
                else return finalQueue
            
            loop conf tNextBus newWaiting (InStop departingBus) queuePenalize

        GroupAbandonment amount -> do
            let totalWaiting = WaitingPassengers (max 0 (getCount waiting - amount))
            putStrLn $ cRed ++ "[" ++ show eventTime ++ "] SE VA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting ++ cReset
            threadDelay (simDelay conf) 
            loop conf tNextBus totalWaiting transitState restQueue

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
