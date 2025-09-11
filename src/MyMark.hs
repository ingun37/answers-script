module MyMark (prefixImageUrl) where

import CMark
import Data.Text qualified as T

prefixImageUrl :: String -> Node -> Node
prefixImageUrl prefix node =
  let safePrefix = T.dropWhileEnd (== '/') (T.pack prefix) <> T.pack "/"
      replaceUrl url = if T.isPrefixOf (T.pack "http") url then url else safePrefix <> T.dropWhile (== '/') url
      recurse (Node posInfo nodeType nodes) =
        case nodeType of
          IMAGE url title -> Node posInfo (IMAGE (replaceUrl url) title) nodes
          _ -> Node posInfo nodeType (recurse <$> nodes)
   in recurse node