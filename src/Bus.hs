module Bus (nextBus, Bus(..)) where

nextBus :: Float -> Maybe Bus
nextBus rand =  if rand>4.5
                    then Just (Bus 20 0)
                    else Nothing

data Bus = Bus 
    { capacity :: Int,
      passengers :: Int
    } deriving (Show)
