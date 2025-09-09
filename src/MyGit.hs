{-# Language OverloadedStrings #-}

module MyGit where

import Git
import Git.Libgit2
import Control.Monad.IO.Class
import Data.Tagged
import Data.Time
import qualified Data.Text as T

myGit :: FilePath -> IO ()
myGit repoPath = do
    let repoOpts = RepositoryOptions { repoPath = "."
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
    withRepository' lgFactory repoOpts $ do
        undefined