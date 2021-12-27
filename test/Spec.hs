module Main where

import Lib (parse)
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import Test.Hspec
import Test.QuickCheck
import qualified System.Directory.Tree as DT
import qualified Data.ByteString as B

toPath :: FilePath
toPath = "test/data/to"

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      removeDirectoryRecursive toPath
      createDirectoryIfMissing True toPath
      parse "test/data/from" toPath
      -- Top directory names are different, hence (_ :/ (Dir _ toDT))
      (_ DT.:/ (DT.Dir _ toDT)) <- DT.readDirectoryWith B.readFile toPath
      (_ DT.:/ (DT.Dir _ beDT)) <- DT.readDirectoryWith B.readFile "test/data/to-be-equal"
      toDT `shouldBe` beDT