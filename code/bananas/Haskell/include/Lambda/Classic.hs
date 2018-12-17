{-

(C) 2010, Ralf Laemmel

This is a simple CBV interpreter.

We use it as a starting point for going after big bananas.

(There is also a free variable analysis.)

-}

module Lambda.Classic where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, insert)
import Data.Set (Set, empty, singleton, union, delete)
import Lambda.Domains


-- The interpreter

eval :: Term -> Env -> Maybe Value

eval (Var x) e = 
  lookup x e

eval (Lambda x t) e =
  Just (InFun (\v -> eval t (insert x v e)))

eval (Apply t t') e = do
  v  <- eval t e
  v' <- eval t' e
  case v of (InFun f) -> f v'; _ -> Nothing

eval Zero _ = 
  Just (InInt 0)

eval (Succ t) e = do
  v <- eval t e
  case v of (InInt i) -> Just (InInt (i+1)); _ -> Nothing

eval (Pred t) e = do
  v <- eval t e
  case v of (InInt i) -> Just (InInt (i-1)); _ -> Nothing

eval (IsZero t) e = do
  v <- eval t e
  case v of (InInt i) -> Just (InBool (i==0)); _ -> Nothing

eval (Cond t t' t'') e = do
  v  <- eval t e
  case v of
    (InBool b) -> if b then eval t' e else eval t'' e
    _ -> Nothing


-- An analysis for free variables

fv :: Term -> Set Name
fv (Var x) = singleton x
fv (Lambda x t) = delete x (fv t) 
fv (Apply t t') = fv t `union` fv t'
fv Zero = empty
fv (Succ t) = fv t
fv (Pred t) = fv t
fv (IsZero t) = fv t
fv (Cond t t' t'') = fv t `union` fv t' `union` fv t''
