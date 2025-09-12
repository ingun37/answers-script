module Main (main) where

import Control.Exception
import Control.Exception (evaluate)
import Control.Monad
import Data.ByteString qualified as B
import Data.Either qualified as E
import Data.Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as Encoding
import Data.Text.IO qualified as TIO
import MyLib qualified
import System.Directory.Tree qualified as DT
import System.FilePath qualified as F
import Test.Hspec
import Test.QuickCheck
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      testCase

src :: FilePath
src = "test" F.</> "answers-db"

dst :: FilePath
dst = "test" F.</> "dst"

expect :: FilePath
expect = "test" F.</> "expect"

prefix :: String
prefix = "prefix"

testCase =
  do
    _ <- MyLib.someFunc prefix src dst
    let reader x = do
          b <- B.readFile x
          return $ E.fromRight (T.pack $ F.takeBaseName x ++ ": " ++ show (B.length b)) $ Encoding.decodeUtf8' b
    a <- DT.readDirectoryWith reader dst
    let a' = a ^.DT._dirTree
    let a'' = DT.flattenDir (set DT._name "" a')
    b <- DT.readDirectoryWith reader expect
    let b' = b ^.DT._dirTree
    let b'' = DT.flattenDir (set DT._name "" b')
    zipWithM_ shouldBe a'' b''