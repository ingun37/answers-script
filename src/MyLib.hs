module MyLib (someFunc) where
import qualified System.Directory.Tree as Dir
import qualified System.FilePath as File
import Control.Lens

someFunc :: String -> FilePath -> FilePath -> IO ()
someFunc prefixPath src dst = do
    root' <- Dir.build src
    let root = over Dir._dirTree (Dir.filterDir theFilter) root'
    print root

theFilter :: Dir.DirTree a -> Bool
theFilter =
    \case
        Dir.Dir name _ -> head name /= '.'
        Dir.File name _ -> ".md" `File.isExtensionOf` name || ".txt" `File.isExtensionOf` name
        _ -> False
