module Passengers (updatePassengers, busInStop, WaitingPassengers) where

import Bus (Bus(capacity, passengers))
import System.Random (randomRIO)

type WaitingPassengers = Int

updatePassengers :: WaitingPassengers -> Float -> IO WaitingPassengers
updatePassengers p lambda = do
                        new <- poisson lambda
                        return (p + new)

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
