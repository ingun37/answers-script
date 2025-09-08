{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc) where

import CMark qualified
import Control.Lens
import Control.Monad qualified as Monad
import Crypto.Hash.SHA1 qualified as SHA (hash)
import Data.ByteString.Base16 qualified as B16 (encode)
import Data.ByteString.Char8 qualified as C8
import Data.Foldable qualified
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
    _files :: Map.Map String FileType
  }
  deriving (Generics.Generic, Show)

makeLenses ''Item

data RefinedDir = RefinedDir {_name :: String, _contents :: [DirTree.DirTree FileType]} deriving (Generics.Generic)

makeLenses ''RefinedDir

refineDir :: DirTree.DirTree FileType -> [RefinedDir]
refineDir = \case DirTree.Dir name contents -> [RefinedDir name contents]; _ -> []

unfolderM :: RefinedDir -> IO (Item, [RefinedDir])
unfolderM d = do
  let subDirs = refineDir =<< (d ^. contents)
  return (convertToItem d, subDirs)

myWriter :: FilePath -> FilePath -> [FilePath] -> Tree.Tree Item -> IO [Effect]
myWriter source destination pathComponents tree = do
  let item = tree ^. TreeLens.root
  let currentPathComponents = pathComponents ++ [item ^. title]
  kids <- mapM (myWriter source destination currentPathComponents) (tree ^. TreeLens.branches)
  let h = sha1InHex $ List.intercalate "/" currentPathComponents
  let hashDir = destination File.</> "sha1" File.</> h
  let writeFileType key =
        \case
          Resource -> do
            let src = File.joinPath $ [source] ++ currentPathComponents ++ [key]
            let dst = hashDir File.</> key
            putStrLn $ "copying " ++ src ++ " -> " ++ dst
            Dir.copyFile src dst
          Attribute (AttributeFile {_posixTime, _content}) -> return ()
  putStrLn $ "Processing: " ++ (take 7 h) ++ "... " ++ File.joinPath currentPathComponents
  _ <- Dir.createDirectoryIfMissing True hashDir
  _ <- Map.traverseWithKey writeFileType (item ^. files)
  let mom = Effect {_hash = h}
  return $ mom : Monad.join kids

someFunc :: String -> FilePath -> FilePath -> IO ()
someFunc prefixPath src dst = do
  root <- DirTree.readDirectoryWithL myReader src
  tree <- Tree.unfoldTreeM unfolderM (head $ refineDir (DirTree.filterDir myFilter (root ^. DirTree._dirTree)))
  effects <- myWriter (File.takeDirectory src) dst [] tree
  print effects

myReader :: FilePath -> IO FileType
myReader path = do
  print path
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

convertToItem :: RefinedDir -> Item
convertToItem d =
  let f = \case DirTree.File filename file -> Map.singleton filename file; _ -> Map.empty
      files = Data.Foldable.foldMap f (d ^. contents)
   in Item
        { _title = d ^. name,
          _files = files
        }