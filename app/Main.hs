module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Exception (finally)
import Parser (getAppConfig)
import Motor (runSimulation)

helpMessage :: String
helpMessage = unlines
    [ "Uso: Autobus [OPCIONES]"
    , ""
    , "Simulador estocástico de una parada de autobús mediante eventos discretos."
    , ""
    , "Opciones disponibles:"
    , "  -h, --help    Muestra este mensaje de ayuda y termina."
    , "  -s   <float>  Media de personas por grupo (Poisson). Por defecto: 1.5"
    , "  -t   <float>  Tasa de llegada de grupos (Exponencial). Por defecto: 0.5"
    , "  -f   <float>  Tiempo base entre autobuses. Por defecto: 5.5"
    , "  -c   <int>    Capacidad máxima del autobús. Por defecto: 30"
    , "  -a   <float>  Parámetro alfa (Beta) de llegada anticipada/coordinada. Por defecto: 4.0"
    , "  -pp  <float>  Sensibilidad de abandono por la multitud en la parada. Por defecto: 0.05"
    , "  -pt  <float>  Sensibilidad de abandono por el tiempo de espera. Por defecto: 0.02"
    , "  -ti  <float>  Tiempo de subida por pasajero. Por defecto: 0.041"
    , "  -to  <float>  Tiempo de bajada por pasajero. Por defecto: 0.02"
    , "  -td  <float>  Tiempo constante de apertura y cierre de puertas. Por defecto: 0.05"
    , "  -pf  <float>  Factor de penalización de paciencia si el autobús está lleno. Por defecto: 3.0"
    , "  -bn  <float>  Factor de ruido base del tiempo de recorrido. Por defecto: 0.4"
    , "  -bdp <float>  Probabilidad de un gran retraso por tráfico. Por defecto: 0.1"
    , "  -bdm <float>  Magnitud del gran retraso. Por defecto: 0.6"
    , "  -dwn <float>  Límite inferior de variabilidad en el tiempo de parada. Por defecto: -0.25"
    , "  -dwx <float>  Límite superior de variabilidad en el tiempo de parada. Por defecto: 0.5"
    , "  -am  <float>  Proporción máxima de pasajeros que desembarcan. Por defecto: 0.5"
    , "  -sd  <int>    Retardo de los hilos en microsegundos (0 para simulación rápida). Por defecto: 250000"
    ]

main :: IO ()
main = do
    args <- getArgs
    
    case args of
        (arg:_) | arg `elem` ["-h", "--help"] -> do
            putStr helpMessage
            exitSuccess
        _ -> case getAppConfig args of 
            Left err -> do
                hPutStrLn stderr $ "\x1b[31m" ++ err ++ "\x1b[0m"
                exitFailure
            Right conf -> do
                runSimulation conf `finally` putStr "\x1b[0m"
