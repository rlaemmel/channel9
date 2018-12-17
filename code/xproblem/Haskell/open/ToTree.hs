-- "Treealize" expression terms

module ToTree where

import Data.Tree
import Data

class ToTree x
 where
  toTree :: x -> Tree String

instance ToTree Const
 where
  toTree (Const i) = Node "Const" []

instance (Expr x, Expr y, ToTree x, ToTree y) => ToTree (Add x y)
 where
  toTree (Add x y) = Node "Add" [toTree x, toTree y]
