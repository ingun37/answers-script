module Main where

import MyLib (someFunc)
import MatlabMark qualified
import Options.Applicative

data GenerateArgs = GenerateArgs
  { prefix :: String,
    src :: String,
    dst :: String
  }

generateParser :: Parser GenerateArgs
generateParser =
  GenerateArgs
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

generateCommand :: ParserInfo GenerateArgs
generateCommand =
  info
    (generateParser <**> helper)
    ( fullDesc
        <> progDesc "Import answers-db into answers static asset"
        <> header "Import answers-db into answers static asset"
    )

data MatlabMarkdownArgs = MatlabMarkdownArgs FilePath FilePath

matlabMarkdownParser :: Parser MatlabMarkdownArgs
matlabMarkdownParser =
  MatlabMarkdownArgs
    <$> strOption
      ( long "src"
          <> help "path to the markdown file that is exported from MATLAB"
      )
    <*> strOption
      ( long "dst"
          <> help "path to a directory in answers-db"
      )

matlabMarkdownCommand :: ParserInfo MatlabMarkdownArgs
matlabMarkdownCommand =
  info
    (matlabMarkdownParser <**> helper)
    ( fullDesc
        <> progDesc "Build assets from a markdown file exported from MATLAB"
        <> header "Build assets from a markdown file exported from MATLAB"
    )

data Args = Generate GenerateArgs | MatlabMarkdown MatlabMarkdownArgs

-- data Args = Args { commandArgs :: CommandArgs }

argsParser :: Parser Args
argsParser =
  hsubparser
    ( command "generate" (Generate <$> generateCommand)
        <> command "matlab-markdown" (MatlabMarkdown <$> matlabMarkdownCommand)
    )

greet :: Args -> IO ()
greet (Generate (GenerateArgs prefix src dst)) = do
  _ <- someFunc prefix src dst
  return ()
greet (MatlabMarkdown (MatlabMarkdownArgs src dst)) = do
  MatlabMark.generateMatlabAnswersDB dst =<< MatlabMark.readMatlabMD src

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
