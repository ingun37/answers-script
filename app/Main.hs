module Main where

import Lib (someFunc)
import Options.Applicative

data Sample = Sample
  { prefixPath :: String,
    src :: String,
    dst :: String
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

greet :: Sample -> IO ()
greet (Sample prefixPath src dst) = someFunc prefixPath src dst

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
