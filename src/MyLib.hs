{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc) where

import CMark qualified
import Control.Lens
import Control.Monad qualified as Monad
import Crypto.Hash.SHA1 qualified as SHA (hash)
import Data.ByteString.Base16 qualified as B16 (encode)
import Data.ByteString.Char8 qualified as C8
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Tree qualified as Tree
import Data.Tree.Lens qualified as TreeLens
import GHC.Generics qualified as Generics
import System.Directory qualified as Dir
import System.Directory.Tree qualified as DirTree
import System.FilePath qualified as File

sha1InHex :: String -> [Char]
sha1InHex = C8.unpack . B16.encode . SHA.hash . C8.pack

data AttributeFile = AttributeFile
  { _posixTime :: Integer,
    _content :: T.Text
  }
  deriving (Generics.Generic, Show)

makeLenses ''AttributeFile

data Effect = Effect
  { _hash :: String
  }
  deriving (Generics.Generic, Show)

data FileType = Resource | Attribute AttributeFile deriving (Generics.Generic, Show)

data Item = Item
  { _title :: String,
    _files :: Map.Map String FileType,
    _parentPathComponents :: [FilePath]
  }
  deriving (Generics.Generic, Show)

makeLenses ''Item

myUnfolder :: ([FilePath], DirTree.DirTree FileType) -> (Item, [([FilePath], DirTree.DirTree FileType)])
myUnfolder (_parentPathComponents, dt) =
  let _title = dt ^. DirTree._name
      contents = dt ^. DirTree._contents
   in ( Item
          { _title,
            _files = Map.fromList [(x, y) | DirTree.File x y <- contents],
            _parentPathComponents
          },
        [(_parentPathComponents ++ [_title], DirTree.Dir x y) | DirTree.Dir x y <- contents]
      )

myWriter :: FilePath -> FilePath -> [FilePath] -> Tree.Tree Item -> IO [Effect]
myWriter source destination pathComponents tree = do
  let item = tree ^. TreeLens.root
  let currentPathComponents = pathComponents ++ [item ^. title]
  let h = sha1InHex $ List.intercalate "/" currentPathComponents

  putStrLn $ "Processing: " ++ take 7 h ++ "... " ++ File.joinPath currentPathComponents

  let hashDir = destination File.</> "sha1" File.</> h

  let copyResource key = do
        let src = File.joinPath $ [source] ++ currentPathComponents ++ [key]
        let dst = hashDir File.</> key
        putStrLn $ "  Copying " ++ src ++ " -> " ++ dst
        Dir.copyFile src dst

  let compileMarkdown key _content = do
        let src = File.joinPath $ [source] ++ currentPathComponents ++ [key]
        let dst = hashDir File.</> key ++ ".html"
        putStrLn $ "  Compiling " ++ src ++ " -> " ++ dst
        TIO.writeFile dst (CMark.commonmarkToHtml [] _content)

  let writeFileType key =
        \case
          Resource -> copyResource key
          Attribute (AttributeFile {_posixTime, _content}) ->
            Monad.when (File.takeExtension key == ".md") $ compileMarkdown key _content
  _ <- Dir.createDirectoryIfMissing True hashDir
  _ <- Map.traverseWithKey writeFileType (item ^. files)
  let mom = Effect {_hash = h}
  kids <- mapM (myWriter source destination currentPathComponents) (tree ^. TreeLens.branches)
  return $ mom : Monad.join kids

someFunc :: String -> FilePath -> FilePath -> IO ()
someFunc prefixPath source destination = do
  root <- DirTree.readDirectoryWithL myReader source
  let refinedDir = DirTree.filterDir myFilter (root ^. DirTree._dirTree)
  let tree = Tree.unfoldTree myUnfolder ([], refinedDir)

  effects <- myWriter (File.takeDirectory source) destination [] tree
  print effects

myReader :: FilePath -> IO FileType
myReader path = do
  let ext = File.takeExtension path
  if ext `elem` [".md", ".txt"]
    then do
      content <- TIO.readFile path
      return $ Attribute $ AttributeFile {_posixTime = 0, _content = content}
    else return Resource

myFilter :: DirTree.DirTree a -> Bool
myFilter =
  \case
    DirTree.Dir name _ -> head name /= '.'
    DirTree.File name _ -> True
    _ -> False
