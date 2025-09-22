{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens
import Control.Monad
import Data.ByteString qualified as B
import Data.Either qualified as E
import Data.Text qualified as T
import Data.Text.Encoding qualified as Encoding
import Data.Text.IO qualified as TIO
import MatlabMark qualified
import MyLib qualified
import System.Directory qualified as D
import System.Directory.Tree qualified as DT
import System.FilePath qualified as F
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "changeMatlabMarkdownDelimeters" $ do
    it "matlab answers markdown test" $ do
      matlab
  describe "MyLib.someFunc" $ do
    it "asset build test" $ do
      testCase

isEmptyDir :: DT.DirTree a -> Bool
isEmptyDir (DT.Dir _ xs) = null xs
isEmptyDir _ = False

makeComparable :: DT.AnchoredDirTree a -> [DT.DirTree a]
makeComparable = filter (not . isEmptyDir) . DT.flattenDir . set DT._name "" . view DT._dirTree

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
    a <- makeComparable <$> DT.readDirectoryWith reader dst
    b <- makeComparable <$> DT.readDirectoryWith reader expect
    zipWithM_ shouldBe a b

matlab :: IO ()
matlab =
  do
    node <- MatlabMark.readMatlabMD $ "test" F.</> "matlab-short.md"
    let dst = "test" F.</> "matlab-dst"
    D.removeDirectoryRecursive dst
    D.createDirectoryIfMissing True dst
    MatlabMark.generateMatlabAnswersDB dst node
    a <- makeComparable <$> DT.readDirectoryWith TIO.readFile dst
    b <- makeComparable <$> DT.readDirectoryWith TIO.readFile ("test" F.</> "matlab-expect")
    zipWithM_ shouldBe a b
