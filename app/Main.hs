module Main where

import MyLib (someFunc)
import Options.Applicative

data Sample = Sample
  { prefix :: String,
    src :: String,
    dst :: String
  }

sample :: Parser Sample
sample =
  Sample
    <$> strOption
      ( long "prefix"
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
greet (Sample prefix src dst) = do
  _ <- someFunc prefix src dst
  return ()

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