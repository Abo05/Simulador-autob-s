module Passengers (getPassengers, busInStop, WaitingPassengers) where

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
            calculateGroups 0 g = return g
            calculateGroups n (times, groupSizes) = do
                beta <- beta1 alpha
                let eventTime = beta * time
                groupSize <- poisson lambdaSize

                calculateGroups (n-1) (eventTime : times, groupSize : groupSizes)



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
