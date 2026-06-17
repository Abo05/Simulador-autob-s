module Passengers (nextPerson, busInStop, WaitingPassengers) where

import Bus (Bus(capacity, passengers))

type WaitingPassengers = Int

nextPerson :: WaitingPassengers -> Float -> WaitingPassengers
nextPerson p rand = p + (round  (abs rand))

busInStop :: Bus -> WaitingPassengers -> WaitingPassengers
busInStop bus =  max 0 . (+ (passengers bus - capacity bus))
