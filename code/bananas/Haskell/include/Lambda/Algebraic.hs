{-

(C) 2010, Ralf Laemmel

This is a CBV interpreter using large bananas.

(There is also a free variable analysis.)

-}

module Lambda.Algebraic where

import Prelude hiding (lookup, pred, succ)
import Data.Monoid
import Data.Map (Map, lookup, insert)
import Data.Set (Set, empty, singleton, union, delete)
import Lambda.Domains


-- The fold algebra

data TermAlgebra r
   = TermAlgebra {
  var    :: String -> r,
  lambda :: String -> r -> r,
  apply  :: r -> r -> r,
  zero   :: r,
  succ   :: r -> r,
  pred   :: r -> r,
  isZero :: r -> r,
  cond   :: r -> r -> r -> r
}


-- The large fold

foldTerm :: TermAlgebra r -> Term -> r
foldTerm a = f
 where
  f (Var x) = var a x
  f (Lambda x t) = lambda a x (f t)
  f (Apply t t') = apply a (f t) (f t')
  f Zero = zero a
  f (Succ t) = succ a (f t)
  f (Pred t) = pred a (f t)
  f (IsZero t) = isZero a (f t)
  f (Cond t t' t'') = cond a (f t) (f t') (f t'')


-- The interpreter

eval :: Term -> Env -> Maybe Value
eval = foldTerm evalAlgebra

evalAlgebra :: TermAlgebra (Env -> Maybe Value)
evalAlgebra = TermAlgebra {
  var = 
    lookup,

  lambda = \x r e -> 
    Just (InFun (\v -> r (insert x v e))),

  apply = \r r' e -> do
    v <- r e
    v' <- r' e
    case v of (InFun f) -> f v'; _ -> Nothing,

  zero = \_ -> 
    Just (InInt 0),

  succ = \r e -> do
    v <- r e
    case v of (InInt i) -> Just (InInt (i+1)); _ -> Nothing,

  pred = \r e -> do
    v <- r e
    case v of (InInt i) -> Just (InInt (i-1)); _ -> Nothing,

  isZero = \r e -> do
    v <- r e
    case v of (InInt i) -> Just (InBool (i==0)); _ -> Nothing,

  cond = \r r' r'' e -> do
    v  <- r e
    case v of
      (InBool b) -> if b then r' e else r'' e
      _ -> Nothing
 
}


-- An analysis for free variables

fv :: Term -> Set Name
fv = foldTerm fvAlgebra

fvAlgebra :: TermAlgebra (Set Name)
fvAlgebra = TermAlgebra {
  var = singleton,
  lambda = delete, 
  apply = union,
  zero = empty,
  succ = id,
  pred = id,
  isZero = id,
  cond = \r r' r'' -> r `union` r' `union` r''
}


-- A monoidal fold algebra

malgebra :: Monoid m => TermAlgebra m
malgebra = TermAlgebra {
  var = \_ -> mempty,
  lambda = \_ r -> r, 
  apply = mappend,
  zero = mempty,
  succ = id,
  pred = id,
  isZero = id,
  cond = \r r' r'' -> r `mappend` r' `mappend` r''
}


-- A second attempt at the free-variable analysis

fv' :: Term -> Set Name
fv' = foldTerm fvAlgebra'

fvAlgebra' :: TermAlgebra (Set Name)
fvAlgebra' = malgebra {
  var = singleton,
  lambda = delete
}
