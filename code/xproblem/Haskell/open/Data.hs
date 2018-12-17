{-# LANGUAGE DeriveDataTypeable #-} 

module Data where

import Data.Generics
import Data.Typeable


-- Data variants for literals and addition

data Const = Const Int                       deriving Typeable
data (Expr l, Expr r) => Add l r = Add l r   deriving Typeable


-- The open union of data variants

class Expr x
instance Expr Const
instance (Expr l, Expr r) => Expr (Add l r)
