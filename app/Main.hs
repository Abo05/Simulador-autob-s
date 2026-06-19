module Main (main) where

import Control.Concurrent (threadDelay)
import Passengers (getPassengers, busInStop, WaitingPassengers)
import Bus (nextBus, Bus(..))
import Events (EventType(..), Event, pushEvent)
import Parser (parse, Config (..))
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

lambdaGroupSize :: Float
lambdaGroupSize = 1.5   -- Media de personas por grupo (Poisson)

lambdaGroupArrival :: Float
lambdaGroupArrival = 1/2   -- Tasa de llegada de grupos (Exponencial)

timeBeetwenBus :: Float
timeBeetwenBus = 5.5  -- Tasa de llegada de autobuses (Uniforme)

preBusCapacity :: Int
preBusCapacity = 30

data AppConfig = AppConfig
    { size     :: Float
    , time     :: Float
    , freq     :: Float
    , cap      :: Int
    } deriving (Show)

applyDefaults :: Config -> AppConfig
applyDefaults cfg = AppConfig
    { size     = fromMaybe lambdaGroupSize    (lambdaSize cfg)
    , time     = fromMaybe lambdaGroupArrival (lambdaTime cfg)
    , freq     = fromMaybe timeBeetwenBus     (busFrequency cfg)
    , cap      = fromMaybe preBusCapacity     (busCapacity cfg)
    }

main :: IO ()
main = do
    args <- getArgs
    let confUsr = parse args
    let conf = applyDefaults confUsr
    
    let fr = freq conf
    let cp = cap conf
    let sz = size conf
    let tm = time conf
    (tFirstGroup, numPassengers) <- getPassengers sz tm
    (tFirstBus, firstBusObj) <- nextBus fr cp
    
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
                                Nothing -> Bus 30 0 -- Fallback de seguridad
            
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
