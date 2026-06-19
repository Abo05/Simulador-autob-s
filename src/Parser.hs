module Parser (Config(..), parse) where 

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
