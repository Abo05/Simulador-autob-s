module Passengers (getPassengers, busInStop, WaitingPassengers) where

import Bus (Bus(capacity, passengers))
import System.Random (randomRIO)
import Events (Time)

type WaitingPassengers = Int

getPassengers :: Float -> Float -> IO (Time, WaitingPassengers)
getPassengers lambdaPoisson lambdaExp = do
                        newPassengers <- poisson lambdaPoisson
                        rand <- randomRIO(0.000001, 1.0)
                        let time = (-log rand) / lambdaExp
                        return  (time, newPassengers)

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

busInStop :: Bus -> WaitingPassengers -> WaitingPassengers
busInStop bus =  max 0 . (+ (passengers bus - capacity bus))
