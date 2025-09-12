module Main (main) where

import Control.Monad
import Data.Map
import MyLib qualified
import System.Directory.Tree qualified as DT
import System.FilePath
import Test.HUnit
import qualified System.Directory.Tree as DT
import qualified Data.ByteString as B
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Encoding qualified as Encoding
import Control.Exception
import Data.Either qualified as E
src :: FilePath
src = "test" </> "answers-db"

dst :: FilePath
dst = "test" </> "dst"

expect :: FilePath
expect = "test" </> "expect"

prefix :: String
prefix = "prefix"

main :: IO ()
main = do
  count <- runTestTT testCase
  print $ showCounts count
testCase =
  TestCase
    ( do
        _ <- MyLib.someFunc prefix src dst
        let reader x = do
              b <- B.readFile x
              return $ E.fromRight (T.pack $ x ++ ": " ++ show (B.length b)) $ Encoding.decodeUtf8' b
        a <- DT.readDirectoryWith reader dst
        b <- DT.readDirectoryWith reader expect
        assertEqual "for the first result of partA," a b
    )
