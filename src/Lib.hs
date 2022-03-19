{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (parse) where

import Crypto.Hash.SHA1 as SHA (hash)
import Data.Aeson (ToJSON)
import Data.Aeson as J (encode)
import Data.Bifunctor (bimap)
import Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Map (Map, fromList)
import qualified Data.Text as Txt (Text, pack, unpack)
import Data.Tree (Tree (Node), flatten, rootLabel)
import GHC.Generics (Generic)
import System.Directory (copyFile, createDirectoryIfMissing, removePathForcibly)
import System.Directory.Tree
  ( DirTree (Dir, File),
    filterDir,
    readDirectoryWithL,
    zipPaths,
  )
import System.FilePath.Posix (dropFileName, isExtensionOf, makeRelative, takeBaseName, takeFileName, (-<.>), (</>))
import Text.Pandoc
  ( Extension (Ext_pipe_tables, Ext_tex_math_dollars, Ext_tex_math_double_backslash),
    HTMLMathMethod (KaTeX),
    def,
    defaultKaTeXURL,
    extensionsFromList,
    readMarkdown,
    readerExtensions,
    runIOorExplode,
    writeHtml5String,
    writerHTMLMathMethod,
  )
import Text.Regex.PCRE.Heavy (gsub, scan)
import Text.Regex.PCRE.Light (compile, dotall, multiline)

data Item = Item
  { title :: String,
    sha1 :: String,
    attr :: Map String String
  }
  deriving (Generic, Show)

data TemTree = TemTree
  { path :: String,
    item :: Item,
    kids :: [Item],
    parentSha1 :: String
  }
  deriving (Generic, Show)

sha1InHex = C8.unpack . B16.encode . SHA.hash . C8.pack

-- Tree, rootLabel, Node, flatten
--it generates tree from bottom-up (dynamic programming)
makeTr :: String -> String -> [DirTree String] -> Tree TemTree
makeTr path parentSha1 entries =
  let sha1 = sha1InHex path
      kidTrs = [makeTr (path </> title) sha1 entries' | Dir title entries' <- entries]
      kidItems = Prelude.map (item . rootLabel) kidTrs
      thisItem = Item (takeFileName path) sha1 (fromList [(takeBaseName name', file) | File name' file <- entries])
      thisNode = TemTree path thisItem kidItems parentSha1
   in Node thisNode kidTrs

instance ToJSON Item

instance ToJSON TemTree

writeJson' :: String -> TemTree -> IO ()
writeJson' dst =
  let f = (dst </>) . (-<.> ".json") . sha1 . item -- make destination file path from Node
      g = toString . J.encode -- make json from Node
   in uncurry writeFile . bimap f g . (\x -> (x, x)) -- dup :: a -> (a,a)

dirFilter :: DirTree a -> Bool
dirFilter (File name _) = ".md" `isExtensionOf` name
dirFilter _ = True

parse :: FilePath -> FilePath -> IO ()
parse src dst = do
  mdDir <- filterDir dirFilter . zipPaths <$> readDirectoryWithL readFile src
  let imgDst = dst </> "imgs"
  let dbDst = dst </> "db"
  let f = \x -> removePathForcibly x >> createDirectoryIfMissing True x -- clean the directory by removing and then making again
  mapM_ f [imgDst, dbDst]
  mdDir' <- traverse (mdTraverse imgDst dst) mdDir
  let (Dir name entries) = mdDir'
  mapM_ (writeJson' dbDst) (flatten (makeTr name "" entries))

mdToHTML :: Txt.Text -> IO Txt.Text
mdToHTML txt =
  runIOorExplode $
    readMarkdown
      def
        { readerExtensions = extensionsFromList [Ext_tex_math_double_backslash, Ext_pipe_tables]
        }
      txt
      >>= writeHtml5String
        def
          { writerHTMLMathMethod = KaTeX defaultKaTeXURL
          }

-- change ![](a/b/c.jpg) to ![](assets/{$1}/c.jpg)
-- copy all the images to assets/$1/
mdTraverse :: FilePath -> FilePath -> (FilePath, String) -> IO String
mdTraverse assetsPath relTo (path, content) = do
  let imgRegex = compile "!\\[\\]\\((?!http)(.+?)\\)" []
  let mdDir = dropFileName path
  let matches = map (head . snd) $ scan imgRegex content
  mapM_ (createDirectoryIfMissing True . (assetsPath </>) . dropFileName) matches
  mapM_ (\m -> copyFile (mdDir </> m) (assetsPath </> m)) matches
  let rel = makeRelative relTo assetsPath
  let md = subDisplayMathBlock $ subInlineMathBlock $ gsub imgRegex (\(d : _) -> "![](/" ++ rel ++ "/" ++ d ++ ")" :: String) content
  html <- mdToHTML (Txt.pack md)
  return $ Txt.unpack html

subInlineMathBlock :: String -> String
subInlineMathBlock =
  let imgRegex = compile "\\$`(.+?)`\\$" []
   in gsub imgRegex (\(d : _) -> "\\\\(" ++ d ++ "\\\\)" :: String)

subDisplayMathBlock :: String -> String
subDisplayMathBlock =
  let imgRegex = compile "^```math$(.+?)^```$" [multiline, dotall]
   in gsub imgRegex (\(d : _) -> "\\\\[" ++ d ++ "\\\\]" :: String)

-- copyFile, createDirectoryIfMissing, removePathForcibly, createDirectoryIfMissing