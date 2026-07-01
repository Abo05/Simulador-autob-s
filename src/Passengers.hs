module Passengers (getPassengers, busInStop, WaitingPassengers, getPatience) where

import Bus (Bus(capacity, passengers))
import System.Random (randomRIO)
import Events (Time)

type WaitingPassengers = Int

getPassengers :: Float -> Float -> Float -> Time  -> IO ([Time], [WaitingPassengers])
getPassengers lambdaSize lambdaArrival alpha time = do
    let u = lambdaArrival * time
    nGroups <- poisson u
    calculateGroups nGroups ([], [])
        where
            calculateGroups :: Int -> ([Time], [WaitingPassengers]) -> IO ([Time], [WaitingPassengers])
            calculateGroups 0 acc = return acc
            calculateGroups n (times, sizes) = do
                groupSize <- poisson lambdaSize
                if groupSize == 0
                    then calculateGroups (n - 1) (times, sizes)
                    else do
                        beta <- beta1 alpha
                        let eventTime = beta * time
                        calculateGroups (n - 1) (eventTime : times, groupSize : sizes)

poisson :: Float -> IO Int
poisson lambda = go 1 0
    where
        limit = exp (-lambda)
        go p n = do
            u <- randomRIO (0.0, 1.0)
            let newP = p * u
            if newP < limit
                then return n
                else go newP (n + 1)

beta1 :: Float -> IO Float
beta1 alpha = do
    rand <- randomRIO (0.0, 1.0)
    let beta = rand ** (1 /alpha)

    return beta

busInStop :: Bus -> WaitingPassengers -> WaitingPassengers
busInStop bus =  max 0 . (+ (passengers bus - capacity bus))

getPatience :: Float -> Float -> WaitingPassengers -> Time -> IO (Maybe Time)
getPatience l1 l2 p t = do 
    let lambda = l1 * fromIntegral p + l2 * t
    
    if lambda <= 0
        then return Nothing
        else do
            rand <- randomRIO (1e-6, 1.0)
            let waitingTime = -(log rand) / lambda
            
            if waitingTime >= t
                then return Nothing
                else return (Just waitingTime)
