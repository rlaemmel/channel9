

{-

The following code deals with nondeterminism (a la Section 2.7) in a
CBN monadic-style interpreter. This variation is suggested in Section
2.9.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup, return, (>>=), fail)

-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b


mzero :: M a
mplus :: M a -> M a -> M a

type L a = [a]

returnL :: a -> L a
returnL a = [a]

bindL :: L a -> (a -> L b) -> L b
m `bindL` k = [ b | a <- m, b <- k a ]

mzeroL :: L a
mzeroL = []

mplusL :: L a -> L a -> L a
l `mplusL` m = l ++ m


type M a = L a
return = returnL
(>>=)  = bindL
mzero  = mzeroL
mplus  = mplusL

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Fail
 | Amb Term Term

data Value =
   Num Int
 | Fun (M Value -> M Value)
 | Wrong

type Environment = [(Name, M Value)]

instance Show Value where
  show (Num i) = show i
  show (Fun _) = "<function>"

  show Wrong = "<wrong>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\m -> interp v ((x,m):e)))
interp (App t u) e = interp t e >>= \f -> apply f (interp u e)
interp Fail e
 = mzero
interp (Amb u v) e
 = interp u e `mplus` interp v e

lookup :: Name -> Environment -> M Value
lookup _ [] = return Wrong
lookup x ((y,n):e) = if x==y then n else lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num (i+j))
add a b = return Wrong

apply :: Value -> M Value -> M Value
apply (Fun h) m = h m
apply f m = return Wrong

test :: Term -> String
test t = show (interp t [])

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termL
 = App (Lam "x" (Add (Var "x") (Var "x")))
       (Amb (Con 2) (Con 1))


main 
 = do
      putStrLn $ test term42
      putStrLn $ test Fail
      putStrLn $ test termL
