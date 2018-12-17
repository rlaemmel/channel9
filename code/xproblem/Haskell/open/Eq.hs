{-# LANGUAGE MultiParamTypeClasses #-}

module Eq where

import Data

class Eq2 x y where
 eq2 :: x -> y -> Bool

instance Eq2 Const Const where
 eq2 (Const i) (Const i')  = i == i'

instance (Expr x, Expr y) => Eq2 Const (Add x y)
 where 
  eq2 _ _ = False

instance (Expr x, Expr y) => Eq2 (Add x y) Const
 where 
  eq2 _ _ = False

instance (Expr x, Expr y, Expr x', Expr y',
          Eq2 x x', Eq2 y y')
 => Eq2 (Add x y) (Add x' y') where
  eq2 (Add x y) (Add x' y') = eq2 x x' && eq2 y y'
