

{-

The following code exercises Section 2.6 of the underlying article.
That section is concerned with lazy output.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup, return, (>>=), fail)

-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b


tell :: Value -> M ()

type O a = (a, String)

returnO :: a -> O a
returnO a = (a, "")

bindO :: O a -> (a -> O b) -> O b
m `bindO` k = let (a,r) = m
                  (b,s) = k a
              in  (b,r++s)

{-

We cannot submit a Show instance so that we would reproduce
the precise output of the original paper. The trouble is that
we would need an overlapping instance, but GHC does not 
allow it for Prelude classes these days.

-}

tellO :: Value -> O ()
tellO a = ((), show a ++ "; ")

type M a = O a
return = returnO
(>>=)  = bindO
tell   = tellO

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Out Term

data Value =
   Num Int
 | Fun (Value -> M Value)
 | Wrong

type Environment = [(Name, Value)]

instance Show Value where
  show (Num i) = show i
  show (Fun _) = "<function>"

  show Wrong = "<wrong>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a
interp (Out u) e
 = interp u e >>= \a ->
   tell a     >>= \() -> 
   return a

lookup :: Name -> Environment -> M Value
lookup _ [] = return Wrong
lookup x ((y,b):e) = if x==y then return b else lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num (i+j))
add a b = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a = return Wrong

test :: Term -> String
test t = show (interp t [])

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termO
 = Add (Out (Con 41)) (Out (Con 1))


main 
 = do
      putStrLn $ test term42
      putStrLn $ test termO
