module Eq where

import Data
-- import Evaluator

{-
instance Eq Expr where
 (Const _) == (Add _ _)   = False
 (Add _ _) == (Const _)   = False
 (Const i) == (Const i')  = i == i'
 (Add x y) == (Add x' y') = x == x' && y == y'

instance Eq Expr where
 x == y = evaluate x == evaluate y
-}
