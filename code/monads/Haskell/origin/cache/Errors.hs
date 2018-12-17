

{-

The following code exercises Section 2.3 of the underlying article.
That section is concerned with the use a kind of error monad for the
purpose of producing useful error messages along interpretation.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup, return, (>>=), fail)

-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b


fail :: String -> M a

data E a
 = Suc a
 | Err String

instance Show (E Value) where
  show (Suc a) = show a
  show (Err s) = "<error: " ++ s ++ ">"

returnE :: a -> E a
returnE a = Suc a

bindE :: E a -> (a -> E b) -> E b
(Suc a) `bindE` k = k a
(Err s) `bindE` k = Err s

failE :: String -> E a
failE s = Err s

type M a = E a
return = returnE
(>>=) = bindE
fail = failE

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term

data Value =
   Num Int
 | Fun (Value -> M Value)

type Environment = [(Name, Value)]

instance Show Value where
  show (Num i) = show i
  show (Fun _) = "<function>"


interp :: Term -> Environment -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a

lookup x [] = fail ("unbound variable: " ++ x) 
lookup x ((y,b):e) = if x==y then return b else lookup x e

add (Num i) (Num j) = return (Num (i+j))
add a b = fail ("should be numbers: "
                ++ show a ++ ","
                ++ show b)

apply (Fun k) a = k a
apply f a = fail ("should be function: " ++ show f)

test :: Term -> String
test t = show (interp t [])

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termE
 = App (Con 1) (Con 2)

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termE
