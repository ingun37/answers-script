{-# LANGUAGE OverloadedStrings #-}

module MyGit (myGit) where

import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIOClass
import Control.Monad.Loops qualified as MonadLoops
import Control.Monad.Trans.Reader qualified as Reader
import Data.ByteString.Char8 qualified as C8
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Tagged qualified as Tagged
import Data.Time qualified as Time
import Git qualified
import Git.Libgit2 qualified as LG

myGit :: FilePath -> IO (Map.Map FilePath Time.ZonedTime)
myGit repoPath = do
  let repoOpts =
        Git.RepositoryOptions
          { repoPath,
            repoWorkingDir = Nothing,
            repoIsBare = False,
            repoAutoCreate = False
          }
  Git.withRepository' LG.lgFactory repoOpts myGit_

myGit_ :: Reader.ReaderT LG.LgRepo IO (Map.Map FilePath Time.ZonedTime)
myGit_ = do
  maybeObjID <- Git.resolveReference "HEAD"
  let commitID = Maybe.fromJust maybeObjID
  headCommit <- Git.lookupCommit (Tagged.Tagged commitID)
  let clone x = (x, x)
  tailCommits <-
    MonadLoops.unfoldrM
      (fmap (fmap clone . Maybe.listToMaybe) . Git.lookupCommitParents)
      headCommit

  MonadIOClass.liftIO $ putStrLn $ "Total commits : " ++ show (length tailCommits)
  MonadIOClass.liftIO $ putStrLn $ "Last  commit  : " ++ show commitID
  MonadIOClass.liftIO $ putStrLn $ "First commit  : " ++ show (Git.commitOid $ last tailCommits)

  seed <- constructEntryTimeMap headCommit

  timeTable' <-
    Monad.foldM
      ( \xMap y -> do
          yMap <- constructEntryTimeMap y
          return $ Map.differenceWith (\_ b -> Just b) xMap yMap
      )
      seed
      tailCommits
  return $ Map.mapKeys (C8.unpack . fst) timeTable'

constructEntryTimeMap :: Git.Commit LG.LgRepo -> Reader.ReaderT LG.LgRepo IO (Map.Map (Git.TreeFilePath, Git.Oid LG.LgRepo) Time.ZonedTime)
constructEntryTimeMap commit = do
  let time = Git.signatureWhen (Git.commitAuthor commit)
  entries <- Git.listTreeEntries True =<< Git.lookupTree (Git.commitTree commit)
  return $ Map.fromList [((x, Tagged.untag oid), time) | (x, Git.BlobEntry oid _) <- entries]
