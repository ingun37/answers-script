{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc) where

import CMark qualified
import Control.Lens
import Control.Monad qualified as Monad
import Crypto.Hash.SHA1 qualified as SHA (hash)
import Data.Aeson as Json
import Data.ByteString.Base16 qualified as B16 (encode)
import Data.ByteString.Char8 qualified as C8
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
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

instance Json.ToJSON AttributeFile where
  toEncoding = Json.genericToEncoding Json.defaultOptions

data PageAttribute = PageAttribute {_time :: Data.Time.ZonedTime, _attributeFile :: AttributeFile} deriving (Generics.Generic)

instance Show PageAttribute where
  show (PageAttribute t af) = show af ++ ", time : " ++ show t

instance Json.ToJSON PageAttribute where
  toEncoding = Json.genericToEncoding Json.defaultOptions

data PageContent = PageContent
  { _pageTitle :: String,
    _hash :: String,
    _attributes :: Map.Map FilePath PageAttribute,
    _answers :: Word
  }
  deriving (Generics.Generic)

makeLenses ''PageContent

instance Json.ToJSON PageContent where
  toEncoding = Json.genericToEncoding Json.defaultOptions

data PageData = PageData
  { _pageContent :: PageContent,
    _parentHash :: String,
    _childPageContents :: [PageContent]
  }
  deriving (Generics.Generic)

makeLenses ''PageData

instance Json.ToJSON PageData where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Show PageData where
  show pg =
    let printEntry (k, v) = k ++ ": " ++ show v
        attribs = NE.nonEmpty (map printEntry (Map.toList (pg ^. pageContent . attributes)))
        attribsStr =
          Maybe.maybe
            ""
            ( \(x NE.:| xs) ->
                "\nattributes  : "
                  ++ x
                  ++ Monad.join (map ("\n              " ++) xs)
            )
            attribs
     in "page title  : "
          ++ pg ^. pageContent . pageTitle
          ++ "\nhash        : "
          ++ pg ^. pageContent . hash
          ++ "\nparent hash : "
          ++ pg ^. parentHash
          ++ "\nanswers     : "
          ++ show (pg ^. pageContent . answers)
          ++ "\nchildren    : "
          ++ show (length (pg ^. childPageContents))
          ++ attribsStr

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
  let _pathComponents = drop 1 parentPathComponents ++ [item ^. title]
  putStrLn $ "Creating hash with " ++ List.intercalate "/" _pathComponents ++ " ..."
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

  Foldable.traverse_ (theWriter source destination) tree
  timeTable <- MyGit.myGit source

  let folder (parentPathComponents, item) children =
        let parentPath = List.intercalate "/" (drop 1 parentPathComponents)
            path = if null parentPath then item ^. title else parentPath ++ "/" ++ item ^. title
            getTime k = Maybe.fromJust $ Map.lookup (path ++ "/" ++ k) timeTable
            _attributes = Map.fromList [(key, PageAttribute {_time = getTime key, _attributeFile}) | (key, Attribute _attributeFile) <- Map.toList (item ^. files)]
         in PageData
              { _pageContent =
                  PageContent
                    { _pageTitle = item ^. title,
                      _hash = sha1InHex path,
                      _attributes,
                      _answers = maybe 0 (const 1) (item ^. files . at "a.md") + sumOf (folded . _head . pageContent . answers) children
                    },
                _parentHash = sha1InHex parentPath,
                _childPageContents = children ^.. folded . _head . pageContent
              }
              : Monad.join children
  let pageDatas = Tree.foldTree folder tree
  let pagesDir = destination File.</> "pages"
  Dir.createDirectoryIfMissing True pagesDir
  let writePageData pg = Json.encodeFile (pagesDir File.</> (pg ^. pageContent . hash) ++ ".json") pg
  Monad.forM_ pageDatas writePageData
  return pageDatas

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
