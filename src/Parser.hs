module Parser (AppConfig(..), getAppConfig) where 

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
    , penFull :: Float
    } deriving (Show)

defaultConfig :: AppConfig
defaultConfig = AppConfig
    { size    = 1.5
    , time    = 0.5
    , freq    = 5.5
    , cap     = 30
    , sep     = 4.0
    , patPass = 0.05
    , patTime = 0.02
    , tIn     = 0.041
    , tOut    = 0.02
    , tDoors  = 0.05
    , penFull = 3.0
    }

parseArgs :: [String] -> AppConfig -> AppConfig
parseArgs [] config               = config
parseArgs ("-s"  : val : xs) conf = parseArgs xs (conf { size    = read val })
parseArgs ("-t"  : val : xs) conf = parseArgs xs (conf { time    = read val })
parseArgs ("-f"  : val : xs) conf = parseArgs xs (conf { freq    = read val })
parseArgs ("-c"  : val : xs) conf = parseArgs xs (conf { cap     = read val })
parseArgs ("-a"  : val : xs) conf = parseArgs xs (conf { sep     = read val })
parseArgs ("-pp" : val : xs) conf = parseArgs xs (conf { patPass = read val })
parseArgs ("-pt" : val : xs) conf = parseArgs xs (conf { patTime = read val })
parseArgs ("-ti" : val : xs) conf = parseArgs xs (conf { tIn     = read val })
parseArgs ("-to" : val : xs) conf = parseArgs xs (conf { tOut    = read val })
parseArgs ("-td" : val : xs) conf = parseArgs xs (conf { tDoors  = read val })
parseArgs ("-pf" : val : xs) conf = parseArgs xs (conf { penFull = read val })
parseArgs (_ : xs)           conf = parseArgs xs conf

getAppConfig :: [String] -> AppConfig
getAppConfig args = parseArgs args defaultConfig
