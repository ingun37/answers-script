{-# LANGUAGE TemplateHaskell #-}

module MyGit
  ( creationTime,
  )
where

import Bindings.Libgit2
import Control.Lens (makeLenses, (<&>), (^.))
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign (Ptr, Storable (peek), alloca)
import Foreign.C.String

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
creationTime fp = withLibGitDo $ do
  withCString fp $ \cfp -> do
    withPointers (func cfp)

func :: CString -> Pointers -> IO (Map FilePath Integer)
func repoPath pointers = do
  c'git_repository_open (pointers ^. repoP) repoPath >>= errorCheck
  repo <- peek $ pointers ^. repoP
  c'git_repository_head (pointers ^. headP) repo >>= errorCheck
  headOid <- peek (pointers ^. headP) >>= c'git_reference_target
  c'git_commit_lookup (pointers ^. commitP) repo headOid >>= errorCheck
  headCommit <- peek (pointers ^. commitP)
  lineage <- unfoldCommits headCommit
  maps <- mapM makeEntryMap lineage
  c'git_repository_free repo
  return $ Map.unionsWith min maps

unfoldCommits :: Ptr C'git_commit -> IO [Ptr C'git_commit]
unfoldCommits commit = alloca $ \parentP -> do
  result <- c'git_commit_parent parentP commit 0
  if result == 0
    then peek parentP >>= unfoldCommits <&> (++ [commit])
    else return [commit]

makeEntryMap'' :: Ptr C'git_repository -> String -> Integer -> Ptr C'git_tree_entry -> IO (Map FilePath Integer)
makeEntryMap'' repo prefix time entry = do
  entryType <- c'git_tree_entry_type entry
  name <- c'git_tree_entry_name entry >>= peekCString
  if entryType == c'GIT_OBJ_TREE
    then do
      eoid <- c'git_tree_entry_id entry
      alloca $ \subTreeP -> do
        c'git_tree_lookup subTreeP repo eoid >>= errorCheck
        subTree <- peek subTreeP
        makeEntryMap' (prefix ++ name ++ "/") time subTree
    else do
      return $ Map.singleton (prefix ++ name) time

makeEntryMap' :: String -> Integer -> Ptr C'git_tree -> IO (Map FilePath Integer)
makeEntryMap' prefix time tree = do
  entryCountC <- c'git_tree_entrycount tree
  let f = c'git_tree_entry_byindex tree
  repo <- c'git_tree_owner tree
  foldMap (f >=> makeEntryMap'' repo prefix time) [0 .. (entryCountC - 1)]

makeEntryMap :: Ptr C'git_commit -> IO (Map FilePath Integer)
makeEntryMap commit = do
  alloca $ \treeP -> do
    c'git_commit_tree treeP commit >>= errorCheck
    tree <- peek treeP
    time <- c'git_commit_time commit
    makeEntryMap' "" (toInteger time) tree

errorCheck r = when (r /= 0) $ error "fail"

-- printMessage = c'git_commit_message >=> peekCString >=> print