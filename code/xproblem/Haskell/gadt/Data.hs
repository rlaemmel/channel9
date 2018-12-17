{-

Based on the discussion at
http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/data-type-extensions.html#gadt-style
we replaced the datatype-based encoding by one that uses GADTs.
Thanks to Niner ShinNoNoir for suggesting such a variation.

-}

{-# LANGUAGE GADTs #-} 

module Data where


-- Data variants for literals and addition

data Const where
 Const :: Int -> Const

data Add l r where
 Add :: (Expr l, Expr r) => l -> r -> Add l r


-- The open union of data variants

class Expr x
instance Expr Const
instance Expr (Add l r)
