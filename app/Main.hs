module Main where

import Data.Semigroup ((<>))
import Lib (parse)
import MyGitHub (runLatestSuccessfulWorkflow)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)

data Sample = Sample
  { prefixPath :: String,
    src :: String,
    dst :: String,
    github :: Bool
  }

sample :: Parser Sample
sample =
  Sample
    <$> strOption
      ( long "prefixpath"
          <> value ""
          <> help "prefix path of webserver"
      )
    <*> strOption
      ( long "src"
          <> help "path to answers-db root directory"
      )
    <*> strOption
      ( long "dst"
          <> help "path to static directory"
      )
    <*> switch
      ( long "github"
          <> help "run the latest successful github workflow of ingun37/answers repository"
      )

greet :: Sample -> IO ()
greet (Sample prefixPath src dst github) = if github then runLatestSuccessfulWorkflow else parse prefixPath src dst

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "import answers-db into answers static asset"
            <> header "what is header?"
        )
