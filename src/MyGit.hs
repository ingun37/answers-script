{-# LANGUAGE OverloadedStrings #-}

module MyGit (myGit) where
import System.Directory qualified as Dir
import System.FilePath ((</>))
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
import Data.ByteString.Char8 qualified as C8

myGit :: FilePath -> IO (Map FilePath ZonedTime)
myGit repoPath = do
  let repoOpts =
        RepositoryOptions
          { repoPath,
            repoWorkingDir = Nothing,
            repoIsBare = False,
            repoAutoCreate = False
          }
  withRepository' LG.lgFactory repoOpts myGit_

myGit_ :: Control.Monad.Trans.Reader.ReaderT LG.LgRepo IO (Map FilePath ZonedTime)
myGit_ = do
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

  timeTable' <-
    foldM
      ( \xMap y -> do
          yMap <- constructEntryTimeMap y
          return $ differenceWith (\_ b -> Just b) xMap yMap
      )
      seed
      tailCommits
  return $ mapKeys (C8.unpack . fst) timeTable'

constructEntryTimeMap :: Commit LG.LgRepo -> ReaderT LG.LgRepo IO (Map (TreeFilePath, Oid LG.LgRepo) ZonedTime)
constructEntryTimeMap commit = do
  let time = signatureWhen (commitAuthor commit)
  entries <- listTreeEntries True =<< lookupTree (commitTree commit)
  return $ fromList [((x, untag oid), time) | (x, BlobEntry oid _) <- entries]
