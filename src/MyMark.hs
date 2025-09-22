{-# LANGUAGE OverloadedStrings #-}

module MyMark (prefixImageUrl) where

import CMarkGFM
import Data.Text qualified as T

tableCellToHTML :: Node -> T.Text
tableCellToHTML (Node _ _ nodes) = "<td>\n" <> T.intercalate "" (map (CMarkGFM.nodeToHtml [] []) (workOnInlineMath nodes)) <> "\n</td>"

tableRowToHTML :: Node -> T.Text
tableRowToHTML (Node _ _ nodes) = "<tr>\n" <> T.intercalate "\n" (map tableCellToHTML nodes) <> "\n</tr>"

tableToInlineHTML :: [TableCellAlignment] -> [Node] -> Node
tableToInlineHTML _ nodes = Node Nothing (HTML_BLOCK $ "<table>\n" <> T.intercalate "\n" (map tableRowToHTML nodes) <> "\n</table>") []

prefixImageUrl :: String -> Node -> Node
prefixImageUrl prefix node =
  let safePrefix = T.pack ("/" ++ prefix ++ "/")
      replaceUrl url = if T.isPrefixOf (T.pack "http") url then url else safePrefix <> T.dropWhile (== '/') url
      recurse (Node _ nodeType nodes) =
        case nodeType of
          IMAGE url title -> Node Nothing (IMAGE (replaceUrl url) title) nodes
          PARAGRAPH -> Node Nothing PARAGRAPH $ workOnInlineMath (map (prefixImageUrl prefix) nodes)
          CODE_BLOCK info text -> if info == T.pack "math" then mathBlock text else Node Nothing nodeType nodes
          TABLE aligns -> tableToInlineHTML aligns nodes
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
  let t = T.pack "\\\\(\n" <> text <> T.pack "\n)\\\\"
   in Node Nothing PARAGRAPH [Node Nothing (TEXT t) []]