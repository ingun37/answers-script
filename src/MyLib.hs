module MyLib (someFunc) where
import qualified System.Directory.Tree as Dir
import qualified System.FilePath as File
import Control.Lens
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

data FileType = Resource FilePath | Attribute T.Text
instance Show FileType where
  show = \case
    Resource _ -> "resource"
    Attribute text -> show text

someFunc :: String -> FilePath -> FilePath -> IO ()
someFunc prefixPath src dst = do
    attributes' <- Dir.readDirectoryWithL myReader src
    let attributes = over Dir._dirTree (Dir.filterDir myFilter) attributes'
    print attributes

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
