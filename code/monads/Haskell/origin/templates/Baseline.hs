{-

The following code corresponds to Fig. 2 (Standard interpreter). That
figure is not complete though, and hence we gather some definitions
from Fig. 1, from which monadic style had to be removed to fit the
types of Fig. 2. Section 2.2 of the underlying paper explains how 
this standard interpreter could be derived from a monadic-style
interpreter (by using the identity monad and performing a number 
of substitutions).

-}

import Prelude hiding (lookup)

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"

data Value =
   Wrong
 | Num Int
 | Fun (Value -> Value)

#include "functions/cba/showval.hs"

#include "types/cbv/Environment.hs"

interp :: Term -> Environment -> Value
interp (Var x) e = lookup x e
interp (Con i) e = Num i
interp (Add u v) e = add (interp u e) (interp v e)
interp (Lam x v) e = Fun (\a -> interp v ((x,a):e))
interp (App t u) e = apply (interp t e) (interp u e)

lookup :: Name -> Environment -> Value
lookup _ [] = Wrong
lookup x ((y,b):e) = if x==y then b else lookup x e

add :: Value -> Value -> Value
add (Num i) (Num j) = Num (i+j)
add _ _ = Wrong

apply :: Value -> Value -> Value
apply (Fun k) a = k a
apply _ _ = Wrong


-- Test the interpreter

#include "terms/42.hs"

main 
 = do
      print $ interp term42 []
