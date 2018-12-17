{-# LANGUAGE GADTs #-} 

module Negation where

import Data
import PrettyPrinter
import Evaluator


-- Data extension for negation

data Neg x where
 Neg :: Expr x => x -> Neg x

instance Expr (Neg x)


-- Extending operation for pretty printing

instance PrettyPrint x => PrettyPrint (Neg x)
 where
  prettyPrint (Neg x) = do putStr "(- "; prettyPrint x; putStr ")"


-- Extending the operation for expression evaluation

instance Evaluate x => Evaluate (Neg x)
 where
  evaluate (Neg x) = 0 - evaluate x
