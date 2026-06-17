module Passengers (nextPerson, busInStop) where

import Bus (Bus(capacity, passengers))

type WaitingPassengers = Int

nextPerson :: WaitingPassengers -> Float -> WaitingPassengers
nextPerson p rand = p + (round rand)

busInStop :: Bus -> WaitingPassengers -> WaitingPassengers
busInStop bus =  max 0 . (+ (passengers bus - capacity bus))
