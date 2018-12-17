module Main where

import Data.Tree
import Data
import PrettyPrinter
import Evaluator
import Eq
import Show
import ToTree
import FromTree

main =
 do
  let l = Const 42
  let a = Add (Const 1) (Const 2)
  print $ prettyPrint l
  print $ prettyPrint a
  print $ evaluate l
  print $ evaluate a
  print $ a == a
  print $ a == l
  print $ a
  print $ toTree a
  print $ fromTree (toTree a)
