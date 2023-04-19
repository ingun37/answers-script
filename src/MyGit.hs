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
func matchPrefix repoPath pointers = do
  c'git_repository_open (pointers ^. repoP) repoPath >>= errorCheck
  repo <- peek $ pointers ^. repoP
  c'git_repository_head (pointers ^. headP) repo >>= errorCheck
  headOid <- peek (pointers ^. headP) >>= c'git_reference_target
  c'git_commit_lookup (pointers ^. commitP) repo headOid >>= errorCheck
  headCommit <- peek (pointers ^. commitP)
  lineage <- unfoldCommits headCommit
  let constructEntryMap' = constructEntrymap matchPrefix repo
  maps <- mapM (getRootAndTime >=> uncurry constructEntryMap') lineage
  c'git_repository_free repo
  return $ Map.unionsWith min maps

getRootAndTime :: Ptr C'git_commit -> IO (Ptr C'git_tree, Integer)
getRootAndTime commit = do
  alloca $ \rootP -> do
    c'git_commit_tree rootP commit >>= errorCheck
    root <- peek rootP
    time <- c'git_commit_time commit
    return (root, toInteger time)

unfoldCommits :: Ptr C'git_commit -> IO [Ptr C'git_commit]
unfoldCommits commit = alloca $ \parentP -> do
  result <- c'git_commit_parent parentP commit 0
  if result == 0
    then peek parentP >>= unfoldCommits <&> (++ [commit])
    else return [commit]

constructEntrymap :: String -> Ptr C'git_repository -> Ptr C'git_tree -> Integer -> IO (Map FilePath Integer)
constructEntrymap matchPrefix repo root time =
  let makeEntryMap'' :: String -> Ptr C'git_tree_entry -> IO (Map FilePath Integer)
      makeEntryMap'' parentDir entry = do
        entryType <- c'git_tree_entry_type entry
        name <- c'git_tree_entry_name entry >>= peekCString
        let next = parentDir ++ name ++ "/"
        if entryType == c'GIT_OBJ_TREE
          then do
            eoid <- c'git_tree_entry_id entry
            alloca $ \subTreeP -> do
              c'git_tree_lookup subTreeP repo eoid >>= errorCheck
              subTree <- peek subTreeP
              if next `isPrefixOf` matchPrefix || matchPrefix `isPrefixOf` next
                then makeEntryMap' (parentDir ++ name ++ "/") subTree
                else return Map.empty
          else do
            if matchPrefix `isPrefixOf` parentDir
              then do
                let relPath = makeRelative matchPrefix (parentDir ++ name)
                return $ Map.singleton relPath time
              else return Map.empty

      makeEntryMap' :: String -> Ptr C'git_tree -> IO (Map FilePath Integer)
      makeEntryMap' parentDir tree = do
        entryCountC <- c'git_tree_entrycount tree
        let f = c'git_tree_entry_byindex tree
        foldMap (f >=> makeEntryMap'' parentDir) [0 .. (entryCountC - 1)]
   in makeEntryMap' "" root

errorCheck r = when (r /= 0) $ error "fail"

-- printMessage = c'git_commit_message >=> peekCString >=> print