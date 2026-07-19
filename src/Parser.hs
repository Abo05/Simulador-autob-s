module Parser (AppConfig(..), getAppConfig) where 

import Text.Read (readMaybe)

data AppConfig = AppConfig
    { size       :: Float
    , time       :: Float
    , freq       :: Float
    , cap        :: Int
    , sep        :: Float
    , patPass    :: Float
    , patTime    :: Float
    , tIn        :: Float
    , tOut       :: Float
    , tDoors     :: Float
    , penFull    :: Float
    , bNoise     :: Float
    , bDelProb   :: Float
    , bDelMax    :: Float
    , dwNMin     :: Float
    , dwNMax     :: Float
    , alightMax  :: Float
    , simDelay   :: Int
    } deriving (Show)

defaultConfig :: AppConfig
defaultConfig = AppConfig
    { size       = 1.5
    , time       = 0.5
    , freq       = 5.5
    , cap        = 30
    , sep        = 4.0
    , patPass    = 0.05
    , patTime    = 0.02
    , tIn        = 0.041
    , tOut       = 0.02
    , tDoors     = 0.05
    , penFull    = 3.0
    , bNoise     = 0.4
    , bDelProb   = 0.1
    , bDelMax    = 0.6
    , dwNMin     = -0.25
    , dwNMax     = 0.5
    , alightMax  = 0.5
    , simDelay   = 250000
    }

parseFloat :: String -> String -> Either String Float
parseFloat flag val = case readMaybe val of
    Just v  -> Right v
    Nothing -> Left $ "Error: Se esperaba un numero valido para la bandera " ++ flag

parseInt :: String -> String -> Either String Int
parseInt flag val = case readMaybe val of
    Just v  -> Right v
    Nothing -> Left $ "Error: Se esperaba un numero entero valido para la bandera " ++ flag

parseArgs :: [String] -> AppConfig -> Either String AppConfig
parseArgs [] config               = Right config
parseArgs ("-s"   : val : xs) conf = parseFloat "-s"   val >>= \v -> parseArgs xs (conf { size       = v })
parseArgs ("-t"   : val : xs) conf = parseFloat "-t"   val >>= \v -> parseArgs xs (conf { time       = v })
parseArgs ("-f"   : val : xs) conf = parseFloat "-f"   val >>= \v -> parseArgs xs (conf { freq       = v })
parseArgs ("-c"   : val : xs) conf = parseInt   "-c"   val >>= \v -> parseArgs xs (conf { cap        = v })
parseArgs ("-a"   : val : xs) conf = parseFloat "-a"   val >>= \v -> parseArgs xs (conf { sep        = v })
parseArgs ("-pp"  : val : xs) conf = parseFloat "-pp"  val >>= \v -> parseArgs xs (conf { patPass    = v })
parseArgs ("-pt"  : val : xs) conf = parseFloat "-pt"  val >>= \v -> parseArgs xs (conf { patTime    = v })
parseArgs ("-ti"  : val : xs) conf = parseFloat "-ti"  val >>= \v -> parseArgs xs (conf { tIn        = v })
parseArgs ("-to"  : val : xs) conf = parseFloat "-to"  val >>= \v -> parseArgs xs (conf { tOut       = v })
parseArgs ("-td"  : val : xs) conf = parseFloat "-td"  val >>= \v -> parseArgs xs (conf { tDoors     = v })
parseArgs ("-pf"  : val : xs) conf = parseFloat "-pf"  val >>= \v -> parseArgs xs (conf { penFull    = v })
parseArgs ("-bn"  : val : xs) conf = parseFloat "-bn"  val >>= \v -> parseArgs xs (conf { bNoise     = v })
parseArgs ("-bdp" : val : xs) conf = parseFloat "-bdp" val >>= \v -> parseArgs xs (conf { bDelProb   = v })
parseArgs ("-bdm" : val : xs) conf = parseFloat "-bdm" val >>= \v -> parseArgs xs (conf { bDelMax    = v })
parseArgs ("-dwn" : val : xs) conf = parseFloat "-dwn" val >>= \v -> parseArgs xs (conf { dwNMin     = v })
parseArgs ("-dwx" : val : xs) conf = parseFloat "-dwx" val >>= \v -> parseArgs xs (conf { dwNMax     = v })
parseArgs ("-am"  : val : xs) conf = parseFloat "-am"  val >>= \v -> parseArgs xs (conf { alightMax  = v })
parseArgs ("-sd"  : val : xs) conf = parseInt   "-sd"  val >>= \v -> parseArgs xs (conf { simDelay   = v })
parseArgs (flag : _)          _    = Left $ "Error: Argumento desconocido o falta un valor para: " ++ flag

validateConfig :: AppConfig -> Either String AppConfig
validateConfig conf
    | size conf       <= 0 = Left "Error: El tamano medio del grupo (-s) debe ser mayor que 0."
    | time conf       <= 0 = Left "Error: La tasa de llegada (-t) debe ser mayor que 0."
    | freq conf       <= 0 = Left "Error: La frecuencia base de autobuses (-f) debe ser mayor que 0."
    | cap conf        <= 0 = Left "Error: La capacidad del autobus (-c) debe ser un entero positivo."
    | sep conf        <= 0 = Left "Error: El parametro de separacion alfa (-a) debe ser mayor que 0."
    | patPass conf    <  0 = Left "Error: La sensibilidad por multitud (-pp) no puede ser negativa."
    | patTime conf    <  0 = Left "Error: La sensibilidad por tiempo (-pt) no puede ser negativa."
    | tIn conf        <= 0 = Left "Error: El tiempo de subida (-ti) debe ser mayor que 0."
    | tOut conf       <= 0 = Left "Error: El tiempo de bajada (-to) debe ser mayor que 0."
    | tDoors conf     <= 0 = Left "Error: El tiempo de puertas (-td) debe ser mayor que 0."
    | penFull conf    <  1 = Left "Error: El factor de penalizacion (-pf) debe ser igual o superior a 1.0."
    | bNoise conf     <  0 = Left "Error: El ruido base (-bn) no puede ser negativo."
    | bDelProb conf < 0 || bDelProb conf > 1 = Left "Error: La probabilidad de retraso (-bdp) debe estar entre 0.0 y 1.0."
    | bDelMax conf    <  0 = Left "Error: La magnitud del retraso (-bdm) no puede ser negativa."
    | dwNMax conf     < dwNMin conf = Left "Error: La variabilidad de parada maxima (-dwx) debe ser mayor que la minima (-dwn)."
    | alightMax conf < 0 || alightMax conf > 1 = Left "Error: El desembarque (-am) debe estar entre 0.0 y 1.0."
    | simDelay conf   <  0 = Left "Error: El retardo (-sd) no puede ser negativo."
    | otherwise            = Right conf

getAppConfig :: [String] -> Either String AppConfig
getAppConfig args = do
    conf <- parseArgs args defaultConfig
    validateConfig conf
