{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MatlabMark (generateMatlabAnswersDB, readMatlabMD) where

import CMark
import CMark.Lens
import Control.Lens
import Data.Attoparsec.Text qualified as A
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.FilePath qualified as File

changeMatlabMarkdownDelimeters :: T.Text -> T.Text
changeMatlabMarkdownDelimeters = T.replace "\n  $$ " "\n```math\n" . T.replace " $$ \n" "\n``` \n" . T.replace "\n $" "\n $`" . T.replace "$\n" "`$\n"

theRecurse :: [Node] -> ([Node], [(String, [Node])])
theRecurse [] = ([], [])
theRecurse (x : xs) =
  let (nodes, pairs) = theRecurse xs
      problemNumber = A.parseOnly parseVersion (x ^. _nodesLens . ix 0 . _nodeType . _TEXT)
   in case problemNumber of
        Left _ -> (x : nodes, pairs)
        Right v -> ([], (v, nodes) : pairs)

groupByProblems :: Node -> ([Node], [(String, [Node])])
groupByProblems (Node _ DOCUMENT nodes) = theRecurse nodes
groupByProblems _ = undefined

parseVersion :: A.Parser String
parseVersion = do
  major <- A.many1 A.digit
  _ <- A.char '.'
  minor <- A.many1 A.digit
  return $ major <> "." <> minor

generateMatlabAnswersDB :: FilePath -> Node -> IO ()
generateMatlabAnswersDB outputDirPath node =
  let (intro, groups) = groupByProblems node
      toDoc = Node Nothing DOCUMENT
      writeMD name nodes = TIO.writeFile (outputDirPath File.</> (name <> ".md")) (CMark.nodeToCommonmark [] Nothing (toDoc nodes))
   in do
        writeMD "cover" intro
        mapM_ (uncurry writeMD) groups

readMatlabMD :: FilePath -> IO Node
readMatlabMD mdFilePath = CMark.commonmarkToNode [] . changeMatlabMarkdownDelimeters <$> TIO.readFile mdFilePath