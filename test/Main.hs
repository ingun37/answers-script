module Main (main) where
import MyLib qualified
import MyGit qualified
import System.FilePath

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
  MyGit.myGit "."