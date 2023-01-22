{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Control.Lens (over, (^.))
import Control.Monad ((<=<))
import qualified Crypto.Hash.SHA1 as SHA (hash)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (readFile)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import Data.ByteString.Lazy (writeFile)
import Data.Map (Map, fromList, lookup)
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (Tree (Node), flatten, rootLabel)
import GHC.Generics (Generic)
import System.Directory (copyFile, createDirectoryIfMissing, removePathForcibly)
import System.Directory.Tree (DirTree (Dir, File), filterDir, readDirectoryWithL, _dirTree)
import System.FilePath (dropFileName, isExtensionOf, makeRelative, takeBaseName, takeExtension, takeFileName, (-<.>), splitDirectories)
import System.FilePath.Posix ( (</>), joinPath )
import Text.Pandoc
  ( Extension (Ext_pipe_tables, Ext_tex_math_double_backslash),
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
import Prelude hiding (lookup, readFile, writeFile)
import Data.Monoid (Sum (Sum), getSum)

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
    parentSha1 :: String,
    numAnswer :: Int
  }
  deriving (Generic, Show)

instance ToJSON Item

instance ToJSON TemTree

sha1InHex :: String -> [Char]
sha1InHex = C8.unpack . B16.encode . SHA.hash . C8.pack

theReader :: String -> String -> String -> FilePath -> IO Text
theReader sitePrefix srcDir dstDir fp = do
  content <- decodeUtf8 <$> readFile fp
  let ext = takeExtension fp
  case ext of
    ".md" -> mdToHTML <=< copyMDMedia sitePrefix srcDir (dropFileName fp) dstDir $ subInlineMathBlock $ subDisplayMathBlock content
    ".txt" -> return content
    _ -> return ""

theFilter :: DirTree a -> DirTree a
theFilter =
  filterDir
    ( \case
        Dir name _ -> head name /= '.'
        File name _ -> ".md" `isExtensionOf` name || ".txt" `isExtensionOf` name
        _ -> False
    )

writeJson :: String -> TemTree -> IO ()
writeJson dst tt = writeFile (dst </> (sha1 (item tt) -<.> ".json")) (encodePretty tt)

someFunc :: String -> FilePath -> FilePath -> IO ()
someFunc prefixPath src dst = do
  anchored <- readDirectoryWithL (theReader prefixPath src dst) src
  let anchored' = over _dirTree theFilter anchored
  let dbDst = dst </> "db"
  removePathForcibly dbDst >> createDirectoryIfMissing True dbDst
  let (Dir name entries) = anchored' ^. _dirTree
  mapM_ (writeJson dbDst) (flatten (makeTr name "" entries))

subInlineMathBlock :: Text -> Text
subInlineMathBlock =
  let imgRegex = compile "\\$`(.+?)`\\$" []
   in gsub imgRegex (\(d : _) -> "\\\\(" ++ d ++ "\\\\)" :: String)

stripString :: String -> String
stripString = unpack . strip . pack

subDisplayMathBlock :: Text -> Text
subDisplayMathBlock =
  let imgRegex = compile "^```math$(.+?)^```$" [multiline, dotall]
   in gsub imgRegex (\(d : _) -> "\\\\[" ++ stripString d ++ "\\\\]" :: String)

copyMDMedia :: String -> FilePath -> FilePath -> FilePath -> Text -> IO Text
copyMDMedia sitePrefix srcDir mdDir dstDir content = do
  let imgRegex = compile "!\\[\\]\\((?!http)(.+?)\\)" []
  let imgNames = map (unpack . head . snd) $ scan imgRegex content
  if null imgNames
    then return content
    else do
      let rel = makeRelative srcDir mdDir
      let relToDst = dstDir </> rel
      createDirectoryIfMissing True relToDst
      mapM_ (\x -> copyFile (mdDir </> x) (relToDst </> x)) imgNames
      let g x = joinPath $ filter (/= "/") ([sitePrefix, rel, x] >>= splitDirectories)
      let f x = mconcat ["![](/", g x, ")"]
      return $ gsub imgRegex (f . head) content

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

countAnswer :: Item -> Sum Int
countAnswer = maybe 0 (const 1) . lookup "a" . attr

makeTr :: String -> String -> [DirTree Text] -> Tree TemTree
makeTr path parentSha1 entries =
  let sha1 = sha1InHex path
      kidTrs = [makeTr (path </> title) sha1 entries' | Dir title entries' <- entries]
      kidItems = map (item . rootLabel) kidTrs
      thisItem = Item (takeFileName path) sha1 (fromList [(takeBaseName name', file) | File name' file <- entries])
      answerNumber = getSum $ countAnswer thisItem <> foldMap (Sum . numAnswer . rootLabel) kidTrs
      thisNode = TemTree path thisItem kidItems parentSha1 answerNumber
   in Node thisNode kidTrs
