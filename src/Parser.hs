module Parser (AppConfig(..), getAppConfig) where 

import Data.Maybe (fromMaybe)

data Config = Config
    { lambdaSize    :: Maybe Float,
      lambdaTime    :: Maybe Float,
      busFrequency  :: Maybe Float,
      busCapacity   :: Maybe Int,
      betaAlpha     :: Maybe Float,
      lambdaPacPass :: Maybe Float,
      lambdaPacTime :: Maybe Float,
      busTimeIn     :: Maybe Float,
      busTimeOut    :: Maybe Float,
      busTimeDoors  :: Maybe Float
    } deriving (Show)

emptyConfig :: Config
emptyConfig = Config Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

parse :: [String] -> Config
parse []   = emptyConfig
parse args = parseArgs args emptyConfig
    where
        parseArgs :: [String] -> Config -> Config
        parseArgs [] config               = config
        parseArgs ("-s": val : xs) config = parseArgs xs (config { lambdaSize = Just (read val) })
        parseArgs ("-t": val : xs) config = parseArgs xs (config { lambdaTime = Just (read val) })
        parseArgs ("-f": val : xs) config = parseArgs xs (config { busFrequency = Just (read val) })
        parseArgs ("-c": val : xs) config = parseArgs xs (config { busCapacity = Just (read val) })
        parseArgs ("-a": val : xs) config = parseArgs xs (config { betaAlpha = Just (read val) })
        parseArgs ("-pp": val : xs) config = parseArgs xs (config { lambdaPacPass = Just (read val) })
        parseArgs ("-pt": val : xs) config = parseArgs xs (config { lambdaPacTime = Just (read val) })
        parseArgs ("-ti": val : xs) config = parseArgs xs (config { busTimeIn = Just (read val) })
        parseArgs ("-to": val : xs) config = parseArgs xs (config { busTimeOut = Just (read val) })
        parseArgs ("-td": val : xs) config = parseArgs xs (config { busTimeDoors = Just (read val) })
        parseArgs (_ : xs) config         = parseArgs xs config

lambdaGroupSize :: Float
lambdaGroupSize = 1.5   -- Media de personas por grupo (Poisson)

lambdaGroupArrival :: Float
lambdaGroupArrival = 1/2   -- Tasa de llegada de grupos (Exponencial)

timeBeetwenBus :: Float
timeBeetwenBus = 5.5  -- Tasa de llegada de autobuses (Uniforme)

preBusCapacity :: Int
preBusCapacity = 30

separation :: Float
separation = 4

patiencePassengers :: Float
patiencePassengers = 0.05

patienceTime :: Float
patienceTime = 0.02

timeInPassenger :: Float
timeInPassenger = 0.041

timeOutPassenger :: Float
timeOutPassenger = 0.02

timeDoorsConst :: Float
timeDoorsConst = 0.05

data AppConfig = AppConfig
    { size    :: Float
    , time    :: Float
    , freq    :: Float
    , cap     :: Int
    , sep     :: Float
    , patPass :: Float
    , patTime :: Float
    , tIn     :: Float
    , tOut    :: Float
    , tDoors  :: Float
    } deriving (Show)

applyDefaults :: Config -> AppConfig
applyDefaults cfg = AppConfig
    { size     = fromMaybe lambdaGroupSize    (lambdaSize cfg)
    , time     = fromMaybe lambdaGroupArrival (lambdaTime cfg)
    , freq     = fromMaybe timeBeetwenBus     (busFrequency cfg)
    , cap      = fromMaybe preBusCapacity     (busCapacity cfg)
    , sep      = fromMaybe separation         (betaAlpha cfg)
    , patPass  = fromMaybe patiencePassengers (lambdaPacPass cfg)
    , patTime  = fromMaybe patienceTime       (lambdaPacTime cfg)
    , tIn      = fromMaybe timeInPassenger    (busTimeIn cfg)
    , tOut     = fromMaybe timeOutPassenger   (busTimeOut cfg)
    , tDoors   = fromMaybe timeDoorsConst     (busTimeDoors cfg)
    }

getAppConfig :: [String] -> AppConfig
getAppConfig = applyDefaults . parse
