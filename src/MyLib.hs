module MyLib (someFunc) where
import qualified System.Directory.Tree as Dir
import qualified System.FilePath as File
import Control.Lens
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified CMark
import qualified Data.Tree as Tree

data FileType = Resource FilePath | Attribute T.Text
instance Show FileType where
  show = \case
    Resource _ -> "resource"
    Attribute text -> show text

someFunc :: String -> FilePath -> FilePath -> IO ()
someFunc prefixPath src dst = do
    root' <- Dir.readDirectoryWithL myReader src
    let root = over Dir._dirTree (Dir.filterDir myFilter) root'
    let unfolder x = fmap (,x^.Dir._contents) (myEffect x)
    tree <- Tree.unfoldTreeM unfolder (root^.Dir._dirTree)
    
    print root

myReader :: FilePath -> IO FileType
myReader path = do
    let ext = File.takeExtension path
    if ext `elem` [".md", ".txt"] 
        then Attribute <$> TIO.readFile path
        else return $ Resource path

myFilter :: Dir.DirTree a -> Bool
myFilter =
    \case
        Dir.Dir name _ -> head name /= '.'
        Dir.File name _ -> True
        _ -> False

myEffect :: Dir.DirTree FileType -> IO ()
myEffect =
    \case
        Dir.Dir name _ -> undefined
        Dir.File name _ -> undefined
        _ -> undefined 