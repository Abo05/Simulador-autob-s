module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitSuccess)
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
    , "  -s  <float>   Media de personas por grupo (Poisson). Por defecto: 1.5"
    , "  -t  <float>   Tasa de llegada de grupos (Exponencial). Por defecto: 0.5"
    , "  -f  <float>   Tiempo base entre autobuses. Por defecto: 5.5"
    , "  -c  <int>     Capacidad máxima del autobús. Por defecto: 30"
    , "  -a  <float>   Parámetro alfa (Beta) de llegada anticipada/coordinada. Por defecto: 4.0"
    , "  -pp <float>   Sensibilidad de abandono por la multitud en la parada. Por defecto: 0.05"
    , "  -pt <float>   Sensibilidad de abandono por el tiempo de espera. Por defecto: 0.02"
    ]

main :: IO ()
main = do
    args <- getArgs
    
    case args of
        (arg:_) | arg `elem` ["-h", "--help"] -> do
            putStr helpMessage
            exitSuccess
        _ -> do
            let conf = getAppConfig args  
            runSimulation conf `finally` putStr "\x1b[0m"
