module Events (EventType(..), Time, Event, pushEvent) where

data EventType = GroupArrival Int 
               | BusArrival
               | GroupAbandonment Time
               | BusDeparture
               deriving (Show)

type Time = Float
type Event = (Time, EventType)

pushEvent :: Event -> [Event] -> [Event]
pushEvent e [] = [e]
pushEvent e1@(t1, _) (e2@(t2,_):xs) = 
    if t1 <= t2 
    then e1 : e2 : xs
    else e2 : pushEvent e1 xs
