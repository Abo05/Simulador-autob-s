module Main (main) where

import System.Environment (getArgs)
import Control.Exception (finally)
import Parser (getAppConfig)
import Motor (runSimulation)

main :: IO ()
main = do
    args <- getArgs
    let conf = getAppConfig args  -- 1. Conseguir configuración limpia
    runSimulation conf `finally` putStr "\x1b[0m"
