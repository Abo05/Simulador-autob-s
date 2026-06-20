module Main (main) where

import System.Environment (getArgs)
import Parser (getAppConfig)
import Motor (runSimulation)

main :: IO ()
main = do
    args <- getArgs
    let conf = getAppConfig args  -- 1. Conseguir configuración limpia
    runSimulation conf            -- 2. Arrancar el motor
