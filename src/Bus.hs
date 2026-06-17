module Bus (nextBus, Bus(..), TimeToNextBus) where

timeBetweenBus :: Int
timeBetweenBus = 7
busCapacity :: Int
busCapacity = 30

data Bus = Bus 
    { capacity :: Int,
      passengers :: Int
    } deriving (Show)
type TimeToNextBus = Int

nextBus :: Float -> TimeToNextBus -> (TimeToNextBus, Maybe Bus)
nextBus rand tm | tm <= 0 = 
                    let nextTime = timeBetweenBus + round (5 * rand)
                        newPassengers = round (fromIntegral busCapacity * abs rand)
                    in (nextTime, Just (Bus busCapacity newPassengers))
                | otherwise = (tm-1, Nothing)
