-- "Treealize" expression terms

module ToTree where

import Data.Tree
import Data

class ToTree x
 where
  toTree :: x -> Tree String

instance ToTree Int
 where
  toTree i = Node (show i) []

instance ToTree Expr
 where
  toTree (Const i) = Node "Const" [toTree i]
  toTree (Add x y) = Node "Add" [toTree x, toTree y]
