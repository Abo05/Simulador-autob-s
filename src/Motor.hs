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

data TransitState = EnRuta Bus | EnParada Bus

delayDeparture :: Time -> [Event] -> [Event]
delayDeparture _ [] = []
delayDeparture delay ((t, BusDeparture) : xs) = pushEvent (t + delay, BusDeparture) xs
delayDeparture delay (x : xs) = x : delayDeparture delay xs

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
    
    loop conf tFirstBus 0 (EnRuta firstBusObj) finalQueue

loop :: AppConfig -> Time -> WaitingPassengers -> TransitState -> [Event] -> IO ()
loop _ _ _ _ [] = return ()
loop conf tNextBus waiting transitState ((eventTime, eventType) : restQueue) = do
    
    case eventType of
        GroupArrival amount -> do
            let totalWaiting = waiting + amount
            putStrLn $ cYellow ++ "[" ++ show eventTime ++ "] LLEGA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting ++ cReset
            
            -- Lógica de embarque dinámico si el bus está estacionado
            (finalWaiting, finalState, queueWithDelay) <- case transitState of
                EnParada parkedBus -> do
                    let (nIn, leftBehind) = boardPassengers parkedBus totalWaiting
                    if nIn > 0
                        then do
                            let updatedBus = parkedBus { passengers = passengers parkedBus + nIn }
                            let delay = tIn conf * fromIntegral nIn
                            putStrLn $ cGreen ++ " -> " ++ show nIn ++ " logran subir corriendo. Retrasan la salida " ++ show delay ++ cReset
                            return (leftBehind, EnParada updatedBus, delayDeparture delay restQueue)
                        else return (totalWaiting, transitState, restQueue)
                EnRuta _ -> return (totalWaiting, transitState, restQueue)

            -- Evaluación de paciencia sobre los que no han logrado subir
            let timeNextBus = max 0 (tNextBus - eventTime)
            patience <- getPatience (patPass conf) (patTime conf) finalWaiting timeNextBus
            
            let queue = case patience of
                            Just waitingTime -> pushEvent (eventTime + waitingTime, GroupAbandonment amount) queueWithDelay
                            Nothing -> queueWithDelay
            
            threadDelay 250000 
            loop conf tNextBus finalWaiting finalState queue
            
        BusArrival _ -> do
            let incomingBus = case transitState of
                                EnRuta b -> b
                                EnParada b -> b
            
            alightFraction <- randomRIO (0.0, 0.5) :: IO Float
            let currentPass = passengers incomingBus
            let nOut = round (alightFraction * fromIntegral currentPass)
            let busAfterAlight = incomingBus { passengers = currentPass - nOut }
            
            let (nIn, newWaiting) = boardPassengers busAfterAlight waiting
            let departingBus = busAfterAlight { passengers = passengers busAfterAlight + nIn }
            
            dwell <- dwellTime nIn nOut (tDoors conf) (tIn conf) (tOut conf)
            
            putStrLn $ cGreen ++ "[" ++ show eventTime ++ "] LLEGA BUS. Bajan: " ++ show nOut ++ 
                       " | Suben: " ++ show nIn ++ " (Dwell: " ++ show dwell ++ ")" ++ cReset
            
            let departureEvent = (eventTime + dwell, BusDeparture)
            let queue1 = pushEvent departureEvent restQueue
            
            -- Generación de pasajeros durante la ventana de puertas abiertas
            groupsDwell <- getPassengers (size conf) (time conf) (sep conf) dwell
            let finalQueue = pushPassengers eventTime groupsDwell queue1
            
            loop conf tNextBus newWaiting (EnParada departingBus) finalQueue

        GroupAbandonment amount -> do
            let totalWaiting = max 0 (waiting - amount)
            putStrLn $ cRed ++ "[" ++ show eventTime ++ "] SE VA GRUPO. Tamaño: " ++ show amount ++ ". Esperando en parada: " ++ show totalWaiting ++ cReset
            threadDelay 250000 
            loop conf tNextBus totalWaiting transitState restQueue

        BusDeparture -> do
            putStrLn $ cGreen ++ "[" ++ show eventTime ++ "] EL BUS SE MARCHA." ++ cReset

            let fr = freq conf
            let cp = cap conf
            (dtNextBus, nextBusObj) <- nextBus fr cp
            
            let absoluteNextBusTime = eventTime + dtNextBus
            let queue1 = pushEvent (absoluteNextBusTime, BusArrival 0) restQueue
            
            groups <- getPassengers (size conf) (time conf) (sep conf) dtNextBus
            let queue2 = pushPassengers eventTime groups queue1
            
            threadDelay 500000 
            loop conf absoluteNextBusTime waiting (EnRuta nextBusObj) queue2
