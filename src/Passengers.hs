module Passengers (
    PassengerGroup(..),
    Passengers,
    totalPassengers,
    pushGroup,
    popGroup,
    removeGroup,
    getPassengers,
    boardPassengers,
    getPatience
) where

import Bus (Bus(capacity, passengers))
import System.Random (randomRIO)
import Events (Time)
import Data.List (partition)

data PassengerGroup = PassengerGroup
    { arrivalTime  :: Time
    , groupSize    :: Int
    , patience     :: Time
    } deriving (Show, Eq)

type Passengers = [PassengerGroup]

-- Total de personas físicas en la cola (suma los tamaños de todos los grupos)
totalPassengers :: Passengers -> Int
totalPassengers = sum . map groupSize

-- Operación uno a uno: Añadir un grupo al final de la cola (pushGroup)
pushGroup :: PassengerGroup -> Passengers -> Passengers
pushGroup g queue = queue ++ [g]

-- Operación uno a uno: Extraer el primer grupo de la cola (popGroup)
popGroup :: Passengers -> Maybe (PassengerGroup, Passengers)
popGroup []     = Nothing
popGroup (x:xs) = Just (x, xs)

-- Operación uno a uno: Buscar y eliminar un grupo específico por su tiempo de llegada
removeGroup :: Time -> Passengers -> (Maybe PassengerGroup, Passengers)
removeGroup tArr queue = case partition (\g -> arrivalTime g == tArr) queue of
    (found:_, rest) -> (Just found, rest)
    ([], rest)      -> (Nothing, rest)

-- Generar todos los grupos del intervalo a la vez
getPassengers :: Float -> Float -> Float -> Time -> IO ([Time], [Int])
getPassengers lambdaSize lambdaArrival alpha time = do
    let u = lambdaArrival * time
    nGroups <- poisson u
    calculateGroups nGroups ([], [])
        where
            calculateGroups :: Int -> ([Time], [Int]) -> IO ([Time], [Int])
            calculateGroups 0 acc = return acc
            calculateGroups n (times, sizes) = do
                sz <- poisson lambdaSize
                if sz == 0
                    then calculateGroups (n - 1) (times, sizes)
                    else do
                        beta <- beta1 alpha
                        let eventTime = beta * time
                        calculateGroups (n - 1) (eventTime : times, sz : sizes)

poisson :: Float -> IO Int
poisson lambda 
    | lambda > 30 = do
        u1 <- randomRIO (1e-6, 1.0)
        u2 <- randomRIO (0.0, 1.0)
        let z0 = sqrt (-2.0 * log u1) * cos (2 * pi * u2)
        let normal = lambda + sqrt lambda * z0
        return $ max 0 (round normal)
    | otherwise = go 1 0
    where
        limit = exp (-lambda)
        go p n = do
            u <- randomRIO (0.0, 1.0)
            let newP = p * u
            if newP <= limit
                then return n
                else go newP (n + 1)

beta1 :: Float -> IO Float
beta1 alpha = do
    rand <- randomRIO (0.0, 1.0)
    let beta = rand ** (1 /alpha)
    return beta

-- Procesar el embarque de grupos indivisibles con paso de cortesía
boardPassengers :: Bus -> Passengers -> (Int, Passengers)
boardPassengers bus queue = go (capacity bus - passengers bus) queue []
  where
    go _ [] acc = (0, reverse acc)
    go space (g:gs) acc
      | space <= 0 = (0, reverse acc ++ (g:gs))
      | groupSize g <= space =
          -- El grupo cabe entero: suben todos y seguimos probando con los de atrás
          let (nIn, rest) = go (space - groupSize g) gs acc
          in (groupSize g + nIn, rest)
      | otherwise =
          -- El grupo NO cabe entero: NO se divide. 
          -- PASO DE CORTESÍA: El grupo se queda a esperar (acc) y probamos con los de atrás (gs)
          go space gs (g : acc)

getPatience :: Float -> Float -> Int -> Time -> IO (Maybe Time)
getPatience l1 l2 waitingCount t = do 
    let lambda = l1 * fromIntegral waitingCount + l2 * t
    if lambda <= 0
        then return Nothing
        else do
            rand <- randomRIO (1e-6, 1.0)
            let waitingTime = -(log rand) / lambda
            if waitingTime >= t
                then return Nothing
                else return (Just waitingTime)
