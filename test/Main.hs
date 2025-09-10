module Main (main) where
import MyLib qualified
import MyGit qualified
import System.FilePath
import Control.Monad
import Data.Map

src :: FilePath
src = "test" </> "src"

dst :: FilePath
dst = "test" </> "dst"

prefix :: String
prefix = "prefix"

main :: IO ()
main = do
  MyLib.someFunc prefix src dst
  putStrLn "Test suite not yet implemented."
  timeTable <- MyGit.myGit "."
  forM_ (toList timeTable) (\(fp, time) -> putStrLn $ show fp ++ " : " ++ show time)
