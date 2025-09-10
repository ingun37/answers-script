{-# LANGUAGE OverloadedStrings #-}

module MyGit (myGit) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Trans.Reader
import Data.Map
import Data.Maybe
import Data.Tagged
import Data.Time
import Git
import Git.Libgit2 qualified as LG

myGit :: FilePath -> IO ()
myGit repoPath = do
  let repoOpts =
        RepositoryOptions
          { repoPath,
            repoWorkingDir = Nothing,
            repoIsBare = False,
            repoAutoCreate = False
          }
  withRepository' LG.lgFactory repoOpts f

f :: Control.Monad.Trans.Reader.ReaderT LG.LgRepo IO ()
f = do
  maybeObjID <- resolveReference "HEAD"
  let commitID = Data.Maybe.fromJust maybeObjID
  headCommit <- lookupCommit (Tagged commitID)
  let clone x = (x, x)
  tailCommits <-
    unfoldrM
      (fmap (fmap clone . listToMaybe) . lookupCommitParents)
      headCommit

  liftIO $ putStrLn $ "Total commits : " ++ show (length tailCommits)
  liftIO $ putStrLn $ "Last  commit  : " ++ show commitID
  liftIO $ putStrLn $ "First commit  : " ++ show (commitOid $ last tailCommits)

  seed <- constructEntryTimeMap headCommit

  timeTable <-
    foldM
      ( \xMap y -> do
          yMap <- constructEntryTimeMap y
          return $ differenceWith (\_ b -> Just b) xMap yMap
      )
      seed
      tailCommits

  liftIO $ forM_ (toList timeTable) (\((fp, _), time) -> putStrLn $ show fp ++ " : " ++ show time)
  undefined

constructEntryTimeMap :: Commit LG.LgRepo -> ReaderT LG.LgRepo IO (Map (TreeFilePath, Oid LG.LgRepo) ZonedTime)
constructEntryTimeMap commit = do
  let treeOid = commitTree commit
  tree <- lookupTree treeOid
  entries <- listTreeEntries True tree
  keys' <- mapM filterBlobEntry entries
  let keys = join keys'
  let author = commitAuthor commit
  let time = signatureWhen author
  let kvs = Prelude.map (,time) keys
  return $ fromList kvs

filterBlobEntry :: (TreeFilePath, TreeEntry LG.LgRepo) -> ReaderT LG.LgRepo IO [(TreeFilePath, Oid LG.LgRepo)]
filterBlobEntry (filePath, entry) =
  case entry of
    BlobEntry entryOid _ -> return [(filePath, untag entryOid)]
    _ -> return []

