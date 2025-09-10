module Main (main) where
import MyLib qualified
import MyGit qualified
import System.FilePath
import Control.Monad
import Data.Map

src :: FilePath
src = "test" </> "answers-db"

dst :: FilePath
dst = "test" </> "dst"

prefix :: String
prefix = "prefix"

main :: IO ()
main = do
  pageDatas <- MyLib.someFunc prefix src dst
  print pageDatas
  forM_ pageDatas print
  timeTable <- MyGit.myGit "."
  forM_ (toList timeTable) (\(fp, time) -> putStrLn $ show fp ++ " : " ++ show time)
