{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.ByteString qualified as B
import Data.Either qualified as E
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Encoding qualified as Encoding
import MyLib qualified
import MatlabMark qualified
import System.Directory.Tree qualified as DT
import System.Directory qualified as D
import System.FilePath qualified as F
import Test.Hspec
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "changeMatlabMarkdownDelimeters" $ do
    it "matlab answers markdown test" $ do
      matlab
  describe "MyLib.someFunc" $ do
    it "asset build test" $ do
      testCase

testCase :: IO ()
testCase =
  do
    let expect = "test" F.</> "expect"
    let dst = "test" F.</> "dst"
    let src = "test" F.</> "answers-db"
    _ <- MyLib.someFunc "prefix" src dst
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

matlab :: IO ()
matlab =
  do
    node <- MatlabMark.readMatlabMD $ "test" F.</> "matlab-short.md"
    let dst = "test" F.</> "matlab-dst"
    D.removeDirectoryRecursive dst
    D.createDirectoryIfMissing True dst
    MatlabMark.generateMatlabAnswersDB dst node
    a <- DT.readDirectoryWith TIO.readFile dst
    let a' = a ^.DT._dirTree
    let a'' = DT.flattenDir (set DT._name "" a')
    b <- DT.readDirectoryWith TIO.readFile ("test" F.</> "matlab-expect")
    let b' = b ^.DT._dirTree
    let b'' = DT.flattenDir (set DT._name "" b')
    zipWithM_ shouldBe a'' b''
