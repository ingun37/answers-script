{-# LANGUAGE TemplateHaskell #-}

module MyGit
  ( creationTime,
  )
where

import Bindings.Libgit2
import Control.Lens (makeLenses, (<&>), (^.))
import Control.Monad (when, (>=>))
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign (Ptr, Storable (peek), alloca)
import Foreign.C.String (CString, peekCString, withCString)
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.FilePath.Posix (takeDirectory, (</>))

-- import Data.Set (Set)
-- import qualified Data.Set as Set
data Pointers = Pointers
  { _repoP :: Ptr (Ptr C'git_repository),
    _headP :: Ptr (Ptr C'git_reference),
    _commitP :: Ptr (Ptr C'git_commit)
  }

makeLenses ''Pointers

withPointers f = alloca $ \x -> do
  alloca $ \y -> do
    alloca $ \z -> do
      f (Pointers x y z)

creationTime :: FilePath -> IO (Map FilePath Integer)
creationTime src = withLibGitDo $ do
  dotGitPath <- canonicalizePath =<< getDotGitPath src
  repoPath <- canonicalizePath $ takeDirectory dotGitPath
  let require = if repoPath == src then "" else makeRelative repoPath src
  withCString dotGitPath $ \csrc -> do
    putStrLn "============================"
    putStrLn $ "src      :" ++ src
    putStrLn $ "require  :" ++ require
    putStrLn $ "repoPath :" ++ repoPath
    putStrLn "============================"
    withPointers (func require csrc)

getDotGitPath :: FilePath -> IO FilePath
getDotGitPath fp = do
  putStrLn $ "testing .git: " ++ fp
  e <- doesDirectoryExist (fp </> ".git")
  let dirName = takeDirectory fp
  when (dirName == fp) $ error "Failed to find .git"
  if e
    then return $ fp </> ".git"
    else getDotGitPath dirName

func :: String -> CString -> Pointers -> IO (Map FilePath Integer)
func require repoPath pointers = do
  c'git_repository_open (pointers ^. repoP) repoPath >>= errorCheck
  repo <- peek $ pointers ^. repoP
  c'git_repository_head (pointers ^. headP) repo >>= errorCheck
  headOid <- peek (pointers ^. headP) >>= c'git_reference_target
  c'git_commit_lookup (pointers ^. commitP) repo headOid >>= errorCheck
  headCommit <- peek (pointers ^. commitP)
  lineage <- unfoldCommits headCommit
  maps <- mapM (makeEntryMap require) lineage
  c'git_repository_free repo
  return $ Map.unionsWith min maps

unfoldCommits :: Ptr C'git_commit -> IO [Ptr C'git_commit]
unfoldCommits commit = alloca $ \parentP -> do
  result <- c'git_commit_parent parentP commit 0
  if result == 0
    then peek parentP >>= unfoldCommits <&> (++ [commit])
    else return [commit]

makeEntryMap'' :: String -> Ptr C'git_repository -> String -> Integer -> Ptr C'git_tree_entry -> IO (Map FilePath Integer)
makeEntryMap'' require repo prefix time entry = do
  entryType <- c'git_tree_entry_type entry
  name <- c'git_tree_entry_name entry >>= peekCString
  if entryType == c'GIT_OBJ_TREE
    then do
      eoid <- c'git_tree_entry_id entry
      alloca $ \subTreeP -> do
        c'git_tree_lookup subTreeP repo eoid >>= errorCheck
        subTree <- peek subTreeP
        let next = prefix ++ name ++ "/"
        if next `isPrefixOf` require || require `isPrefixOf` next
          then makeEntryMap' require (prefix ++ name ++ "/") time subTree
          else return Map.empty
    else do
      if require `isPrefixOf` prefix
        then do
          let relPath = makeRelative require (prefix ++ name)
          return $ Map.singleton relPath time
        else return Map.empty

makeEntryMap' :: String -> String -> Integer -> Ptr C'git_tree -> IO (Map FilePath Integer)
makeEntryMap' require prefix time tree = do
  entryCountC <- c'git_tree_entrycount tree
  let f = c'git_tree_entry_byindex tree
  repo <- c'git_tree_owner tree
  foldMap (f >=> makeEntryMap'' require repo prefix time) [0 .. (entryCountC - 1)]

makeEntryMap :: String -> Ptr C'git_commit -> IO (Map FilePath Integer)
makeEntryMap require commit = do
  alloca $ \treeP -> do
    c'git_commit_tree treeP commit >>= errorCheck
    tree <- peek treeP
    time <- c'git_commit_time commit
    makeEntryMap' require "" (toInteger time) tree

errorCheck r = when (r /= 0) $ error "fail"

-- printMessage = c'git_commit_message >=> peekCString >=> print