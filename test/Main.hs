module Main (main) where

import qualified MyLib

src :: FilePath
src = "test/src"

dst :: FilePath
dst = "test/dst"

prefix :: String
prefix = "prefix"

main :: IO ()
main = do
    MyLib.someFunc prefix src dst
    putStrLn "Test suite not yet implemented."
