{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Text (Text)
import qualified Data.Text as Txt (concat, pack, strip, unpack)
import qualified Data.Text.IO as TxtIO (readFile)
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
    githubMarkdownExtensions,
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
    attr :: Map String Text
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
makeTr :: String -> String -> [DirTree Text] -> Tree TemTree
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

myFilter =
  filterDir
    ( \case
        (Dir name _) -> head name /= '.'
        (File name _) -> ".md" `isExtensionOf` name
        _ -> True
    )

data AllowedFileType = Markdown String | PlainText String

parse :: FilePath -> FilePath -> IO ()
parse src dst = do
  anchored <- readDirectoryWithL TxtIO.readFile src
  let anchorRemoved = zipPaths anchored
  let mdDir = myFilter anchorRemoved
  let imgDst = dst </> "imgs"
  let dbDst = dst </> "db"
  let f = \x -> removePathForcibly x >> createDirectoryIfMissing True x -- clean the directory by removing and then making again
  mapM_ f [imgDst, dbDst]
  mdDir' <- traverse (mdTraverse imgDst dst) mdDir
  let (Dir name entries) = mdDir'
  mapM_ (writeJson' dbDst) (flatten (makeTr name "" entries))

mdToHTML :: Text -> IO Text
mdToHTML txt =
  runIOorExplode $
    readMarkdown
      def
        { readerExtensions = githubMarkdownExtensions <> extensionsFromList [Ext_tex_math_double_backslash, Ext_pipe_tables]
        }
      txt
      >>= writeHtml5String
        def
          { writerHTMLMathMethod = KaTeX defaultKaTeXURL
          }

-- change ![](a/b/c.jpg) to ![](assets/{$1}/c.jpg)
-- copy all the images to assets/$1/
mdTraverse :: FilePath -> FilePath -> (FilePath, Text) -> IO Text
mdTraverse assetsPath relTo (path, content) = do
  let imgRegex = compile "!\\[\\]\\((?!http)(.+?)\\)" []
  let mdDir = dropFileName path
  let matches = map (Txt.unpack . head . snd) $ scan imgRegex content :: [FilePath]
  mapM_ (createDirectoryIfMissing True . (assetsPath </>) . dropFileName) matches
  mapM_ (\m -> copyFile (mdDir </> m) (assetsPath </> m)) matches
  let rel = makeRelative relTo assetsPath
  let subImageTag = gsub imgRegex (\(d : _) -> Txt.concat ["![](/", Txt.pack rel, "/", d, ")"] :: Text)
  let md = subDisplayMathBlock $ subInlineMathBlock $subImageTag content
  mdToHTML md

subInlineMathBlock :: Text -> Text
subInlineMathBlock =
  let imgRegex = compile "\\$`(.+?)`\\$" []
   in gsub imgRegex (\(d : _) -> "\\\\(" ++ d ++ "\\\\)" :: String)

stripString = Txt.unpack . Txt.strip . Txt.pack

subDisplayMathBlock :: Text -> Text
subDisplayMathBlock =
  let imgRegex = compile "^```math$(.+?)^```$" [multiline, dotall]
   in gsub imgRegex (\(d : _) -> "\\\\[" ++ stripString d ++ "\\\\]" :: String)

-- copyFile, createDirectoryIfMissing, removePathForcibly, createDirectoryIfMissing