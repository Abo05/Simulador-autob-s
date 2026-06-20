module Parser (AppConfig(..), getAppConfig) where 

import Data.Maybe (fromMaybe)

data Config = Config
    { lambdaSize    :: Maybe Float,
      lambdaTime    :: Maybe Float,
      busFrequency  :: Maybe Float,
      busCapacity   :: Maybe Int
    } deriving (Show)

emptyConfig :: Config
emptyConfig = Config Nothing Nothing Nothing Nothing

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
        parseArgs (_ : xs) config         = parseArgs xs config

lambdaGroupSize :: Float
lambdaGroupSize = 1.5   -- Media de personas por grupo (Poisson)

lambdaGroupArrival :: Float
lambdaGroupArrival = 1/2   -- Tasa de llegada de grupos (Exponencial)

timeBeetwenBus :: Float
timeBeetwenBus = 5.5  -- Tasa de llegada de autobuses (Uniforme)

preBusCapacity :: Int
preBusCapacity = 30

data AppConfig = AppConfig
    { size     :: Float
    , time     :: Float
    , freq     :: Float
    , cap      :: Int
    } deriving (Show)

applyDefaults :: Config -> AppConfig
applyDefaults cfg = AppConfig
    { size     = fromMaybe lambdaGroupSize    (lambdaSize cfg)
    , time     = fromMaybe lambdaGroupArrival (lambdaTime cfg)
    , freq     = fromMaybe timeBeetwenBus     (busFrequency cfg)
    , cap      = fromMaybe preBusCapacity     (busCapacity cfg)
    }

getAppConfig :: [String] -> AppConfig
getAppConfig = applyDefaults . parse
