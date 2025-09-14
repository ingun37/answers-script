module Main where

import MyLib (someFunc)
import Options.Applicative

data BuildAssetArgs = BuildAssetArgs
  { prefix :: String,
    src :: String,
    dst :: String
  }

buildAssetParser :: Parser BuildAssetArgs
buildAssetParser =
  BuildAssetArgs
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

buildAssetCommand :: ParserInfo BuildAssetArgs
buildAssetCommand =
  info
    (buildAssetParser <**> helper)
    ( fullDesc
        <> progDesc "Import answers-db into answers static asset"
        <> header "Import answers-db into answers static asset"
    )

data SplitMatlabMarkdownArgs = SplitMatlabMarkdownArgs FilePath FilePath

splitMatlabMarkdownParser :: Parser SplitMatlabMarkdownArgs
splitMatlabMarkdownParser =
  SplitMatlabMarkdownArgs
    <$> strOption
      ( long "src"
          <> help "path to the markdown file that is exported from MATLAB"
      )
    <*> strOption
      ( long "dst"
          <> help "path to a directory"
      )

splitMatlabMarkdownCommand :: ParserInfo SplitMatlabMarkdownArgs
splitMatlabMarkdownCommand =
  info
    (splitMatlabMarkdownParser <**> helper)
    ( fullDesc
        <> progDesc "Build assets from a markdown file exported from MATLAB"
        <> header "Build assets from a markdown file exported from MATLAB"
    )

data Args = BuildAssets BuildAssetArgs | SplitMatlabMarkdown SplitMatlabMarkdownArgs

-- data Args = Args { commandArgs :: CommandArgs }

argsParser :: Parser Args
argsParser =
  hsubparser
    ( command "build-assets" (BuildAssets <$> buildAssetCommand)
        <> command "split-matlab-markdown" (SplitMatlabMarkdown <$> splitMatlabMarkdownCommand)
    )

greet :: Args -> IO ()
greet (BuildAssets (BuildAssetArgs prefix src dst)) = do
  _ <- someFunc prefix src dst
  return ()
greet (SplitMatlabMarkdown (SplitMatlabMarkdownArgs src dst)) = do
  return ()

main :: IO ()
main =
  greet
    =<< execParser
      ( info
          (argsParser <**> helper)
          ( fullDesc
              <> progDesc "Collection of utility functions for the Answers web app"
              <> header "Collection of utility functions for the Answers web app"
          )
      )