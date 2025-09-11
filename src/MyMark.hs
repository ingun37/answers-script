module MyMark (prefixImageUrl) where

import CMark
import Data.Text qualified as T

prefixImageUrl :: String -> Node -> Node
prefixImageUrl prefix node =
  let safePrefix = T.dropWhileEnd (== '/') (T.pack prefix) <> T.pack "/"
      replaceUrl url = if T.isPrefixOf (T.pack "http") url then url else safePrefix <> T.dropWhile (== '/') url
      recurse (Node posInfo nodeType nodes) =
        case nodeType of
          IMAGE url title -> Node Nothing (IMAGE (replaceUrl url) title) nodes
          PARAGRAPH -> Node Nothing PARAGRAPH $ workOnInlineMath nodes
          CODE_BLOCK info text -> if info == T.pack "math" then mathBlock text else Node Nothing nodeType nodes
          _ -> Node Nothing nodeType (recurse <$> nodes)
   in recurse node

workOnInlineMath :: [Node] -> [Node]
workOnInlineMath (x : y : z : tail) =
  case (x, y, z) of
    (Node _ (TEXT l) [], Node _ (CODE m) [], Node _ (TEXT r) []) ->
      if T.isSuffixOf (T.pack "$") l && T.isPrefixOf (T.pack "$") r
        then x : Node Nothing (TEXT (T.pack "`" <> m <> T.pack "`")) [] : workOnInlineMath (z : tail)
        else x : y : workOnInlineMath (z : tail)
    _ -> x : workOnInlineMath (y : z : tail)
workOnInlineMath (x : xs) = x : workOnInlineMath xs
workOnInlineMath [] = []

mathBlock :: T.Text -> Node
mathBlock text =
  let t = T.pack "\\\\(" <> text <> T.pack ")\\\\"
   in Node Nothing PARAGRAPH [Node Nothing (TEXT t) []]