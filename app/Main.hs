module Main where

import System.Environment (getArgs)
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Lib (parse)

main = do
    args <- getArgs
    case args of (src:dst:[]) -> parse src dst
                 _ -> putStrLn "Usage: json-generator <src> <dst>" >> exitWith ExitSuccess