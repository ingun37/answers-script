{-# LANGUAGE DeriveGeneric #-}

module MyLib (someFunc) where

import CMark qualified
import Control.Lens
import Data.Foldable qualified
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Tree qualified as Tree
import GHC.Generics qualified as Generics
import System.Directory.Tree qualified as Dir
import System.FilePath qualified as File

data AttributeFile = AttributeFile
  { posixTime :: Integer,
    content :: T.Text
  }
  deriving (Generics.Generic, Show)

data Item = Item
  { title :: String,
    sha1 :: String,
    attr :: Map.Map String AttributeFile,
    numAnswer :: Int
  }
  deriving (Generics.Generic, Show)

data FileType = Resource FilePath | Attribute AttributeFile deriving (Generics.Generic)

isDir :: Dir.DirTree a -> Bool
isDir = \case Dir.Dir _ _ -> True; _ -> False

someFunc :: String -> FilePath -> FilePath -> IO ()
someFunc prefixPath src dst = do
  root' <- Dir.readDirectoryWithL myReader src
  let root = over Dir._dirTree (Dir.filterDir myFilter) root'
  let unfolderM x = fmap (,filter isDir (x ^. Dir._contents)) (convertToItem x)
  tree <- Tree.unfoldTreeM unfolderM (root ^. Dir._dirTree)
  print tree

myReader :: FilePath -> IO FileType
myReader path = do
  let ext = File.takeExtension path
  let process = if ext == ".md" then CMark.commonmarkToHtml [] else id
  if ext `elem` [".md", ".txt"]
    then do
      content <- TIO.readFile path
      return $ Attribute $ AttributeFile {posixTime = 0, content = process content}
    else return $ Resource path

myFilter :: Dir.DirTree a -> Bool
myFilter =
  \case
    Dir.Dir name _ -> head name /= '.'
    Dir.File name _ -> True
    _ -> False

convertToItem :: Dir.DirTree FileType -> IO Item
convertToItem =
  \case
    Dir.Dir name contents -> do
      let f = \case Dir.File filename (Attribute af) -> Map.singleton filename af; _ -> Map.empty
      let attributes = Data.Foldable.foldMap f contents
      return
        Item
          { title = name,
            sha1 = "",
            attr = attributes,
            numAnswer = 0
          }
    _ -> undefined