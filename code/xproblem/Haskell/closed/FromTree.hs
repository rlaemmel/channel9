-- "Treealize" expression terms

module FromTree where

import Data.Tree
import Data

fromTree :: Tree String -> Expr
fromTree (Node "Const" [i]) =
 let (Node i' []) = i in
  Const (read i')
fromTree (Node "Add" [x,y]) =
  Add x' y'
 where
  x' = fromTree x
  y' = fromTree y
