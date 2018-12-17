module Evaluator where

import Data


-- Operation for expression evaluation

class Expr x => Evaluate x 
 where
  evaluate :: x -> Int

instance Evaluate Const
 where
  evaluate (Const i) = i

instance (Evaluate l, Evaluate r) => Evaluate (Add l r)
 where 
  evaluate (Add l r) = evaluate l + evaluate r
