{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc) where

import CMark qualified
import Control.Lens
import Control.Lens.Fold qualified as FoldLens
import Control.Monad qualified as Monad
import Crypto.Hash.SHA1 qualified as SHA (hash)
import Data.ByteString.Base16 qualified as B16 (encode)
import Data.ByteString.Char8 qualified as C8
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time qualified
import Data.Tree qualified as Tree
import Data.Tree.Lens qualified as TreeLens
import GHC.Generics qualified as Generics
import MyGit qualified
import System.Directory qualified as Dir
import System.Directory.Tree qualified as DirTree
import System.FilePath qualified as File

sha1InHex :: String -> [Char]
sha1InHex = C8.unpack . B16.encode . SHA.hash . C8.pack

data AttributeFile = AttributeFile
  { _content :: T.Text
  }
  deriving (Generics.Generic)

makeLenses ''AttributeFile

instance Show AttributeFile where
  show (AttributeFile c) = "(" ++ show (T.length c) ++ " long text)"

data PageAttribute = PageAttribute {_time :: Data.Time.ZonedTime, _attributeFile :: AttributeFile} deriving (Generics.Generic)

instance Show PageAttribute where
  show (PageAttribute t af) = show af ++ ", time : " ++ show t

data PageData = PageData
  { _hash :: String,
    _attributes :: Map.Map FilePath PageAttribute,
    _answers :: Word
  }
  deriving (Generics.Generic)

makeLenses ''PageData

instance Show PageData where
  show (PageData x y z) =
    let printEntry (k, v) = "  " ++ k ++ ": " ++ show v
     in "hash    : "
          ++ x
          ++ "\n"
          ++ "answers : "
          ++ show z
          ++ "\n"
          ++ "attributes:\n"
          ++ List.intercalate "\n" (map printEntry (Map.toList y))

data FileType = Resource | Attribute AttributeFile deriving (Generics.Generic, Show)

data Item = Item
  { _title :: String,
    _files :: Map.Map String FileType
  }
  deriving (Generics.Generic, Show)

makeLenses ''Item

myUnfolder :: DirTree.DirTree FileType -> (Item, [DirTree.DirTree FileType])
myUnfolder dt =
  let _title = dt ^. DirTree._name
      contents = dt ^. DirTree._contents
   in ( Item
          { _title,
            _files = Map.fromList [(x, y) | DirTree.File x y <- contents]
          },
        [DirTree.Dir x y | DirTree.Dir x y <- contents]
      )

zipPath :: Tree.Tree Item -> Tree.Tree ([FilePath], Item)
zipPath t =
  let item = t ^. TreeLens.root
      branches = t ^. TreeLens.branches
      prepend = (item ^. title :)
      mapPrepend = over (mapped . _1) prepend
      branches' = over mapped (mapPrepend . zipPath) branches
   in Tree.Node ([], item) branches'

theWriter :: FilePath -> FilePath -> ([FilePath], Item) -> IO ()
theWriter source destination (parentPathComponents, item) = do
  let _pathComponents = parentPathComponents ++ [item ^. title]
  let _hash = sha1InHex $ List.intercalate "/" _pathComponents

  putStrLn $ "Processing: " ++ take 7 _hash ++ "... " ++ File.joinPath _pathComponents

  let hashDir = destination File.</> "sha1" File.</> _hash

  let copyResource key = do
        let src = File.joinPath $ [source] ++ _pathComponents ++ [key]
        let dst = hashDir File.</> key
        putStrLn $ "  Copying " ++ src ++ " -> " ++ dst
        Dir.copyFile src dst

  let compileMarkdown key _content = do
        let src = File.joinPath $ [source] ++ _pathComponents ++ [key]
        let dst = hashDir File.</> key ++ ".html"
        putStrLn $ "  Compiling " ++ src ++ " -> " ++ dst
        TIO.writeFile dst (CMark.commonmarkToHtml [] _content)

  let writeFileType key =
        \case
          Resource -> copyResource key
          Attribute (AttributeFile {_content}) ->
            Monad.when (File.takeExtension key == ".md") $ compileMarkdown key _content
  _ <- Dir.createDirectoryIfMissing True hashDir
  _ <- Map.traverseWithKey writeFileType (item ^. files)
  return ()

someFunc :: String -> FilePath -> FilePath -> IO [PageData]
someFunc prefixPath source destination = do
  root <- DirTree.readDirectoryWithL myReader source

  let tree = zipPath $ Tree.unfoldTree myUnfolder (DirTree.filterDir myFilter $ root ^. DirTree._dirTree)

  Foldable.traverse_ (theWriter (File.takeDirectory source) destination) tree
  timeTable <- MyGit.myGit source

  let folder (parentPathComponents, item) children =
        let pathComponents = drop 1 parentPathComponents ++ [item ^. title]
            path = List.intercalate "/" pathComponents
            _hash = sha1InHex $ path
            _time = Maybe.fromJust $ Map.lookup path timeTable
            getTime k = Maybe.fromJust $ Map.lookup (List.intercalate "/" (pathComponents ++ [k])) timeTable
            _attributes = Map.fromList [(key, PageAttribute {_time = getTime key, _attributeFile}) | (key, Attribute _attributeFile) <- Map.toList (item ^. files)]
         in PageData
              { _hash,
                _answers = maybe 0 (const 1) (item ^. files . at "a.md") + sumOf (folded . folded . answers) children,
                _attributes
              }
              : Monad.join children
  return $ Tree.foldTree folder tree

myReader :: FilePath -> IO FileType
myReader path = do
  let ext = File.takeExtension path
  if ext `elem` [".md", ".txt"]
    then do
      _content <- TIO.readFile path
      return $ Attribute $ AttributeFile {_content}
    else return Resource

myFilter :: DirTree.DirTree a -> Bool
myFilter =
  \case
    DirTree.Dir name _ -> head name /= '.'
    DirTree.File name _ -> head name /= '.'
    _ -> False
