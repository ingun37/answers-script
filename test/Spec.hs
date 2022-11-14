import Lib

import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing, doesDirectoryExist)
import Test.Hspec
import Test.QuickCheck
import qualified System.Directory.Tree as DT
import qualified Data.ByteString as B
import Control.Monad (when)
import System.FilePath (takeExtension, takeFileName)
src :: FilePath
src = "test/src"

dst :: FilePath
dst = "test/dst"

prefix :: String
prefix = "prefix"

expect :: FilePath
expect = "test/expect"

simpleReader :: FilePath -> IO String
simpleReader fp = case takeExtension fp of
  ".json" -> readFile fp
  _ -> return (takeFileName fp)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      dstExist <- doesDirectoryExist dst
      when dstExist (removeDirectoryRecursive dst)
      createDirectoryIfMissing True dst
      someFunc prefix src dst
      -- Top directory names are different, hence (_ :/ (Dir _ toDT))
      (_ DT.:/ (DT.Dir _ toDT)) <- DT.readDirectoryWith simpleReader dst
      (_ DT.:/ (DT.Dir _ beDT)) <- DT.readDirectoryWith simpleReader expect
      toDT `shouldBe` beDT