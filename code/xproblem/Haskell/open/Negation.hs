{-# LANGUAGE DeriveDataTypeable #-} 

module Negation where

import Data.Tree
import Data.Generics
import Data.Typeable
import Data
import PrettyPrinter
import Evaluator
import ToTree


-- Data extension for negation

data Expr x => Neg x = Neg x       deriving (Typeable, Data)

instance Expr x => Expr (Neg x)


-- Extending operation for pretty printing

instance PrettyPrint x => PrettyPrint (Neg x)
 where
  prettyPrint (Neg x) = do putStr "(- "; prettyPrint x; putStr ")"


-- Extending the operation for expression evaluation

instance Evaluate x => Evaluate (Neg x)
 where
  evaluate (Neg x) = 0 - evaluate x


-- Extending the operation show

instance (Expr x, Show x) => Show (Neg x)
 where
  show (Neg x) = "Neg (" ++ show x ++ ")"


-- Extending the operation toTree

instance (Expr x, ToTree x) => ToTree (Neg x)
 where
  toTree (Neg x) = Node "Neg" [toTree x]
