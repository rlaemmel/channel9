module Data where

data Expr = Const Int
          | Add Expr Expr
     deriving (Eq,Show,Read)
