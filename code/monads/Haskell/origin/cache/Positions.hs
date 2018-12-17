

{-

The following code exercises Section 2.4 of the underlying article.
That section is concerned with term positions for the benefit of
improving error messages along interpretation.

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

type P a = Position -> E a

returnP :: a -> P a
returnP a = \p -> returnE a

bindP :: P a -> (a -> P b) -> P b
m `bindP` k = \p -> m p `bindE` \x -> k x p

instance Show (P Value) where
  show f = show (f pos0)

failP :: String -> P a
failP s = \p -> failE (show p ++ ": " ++ s)

resetP :: Position -> P x -> P x
resetP q m = \p -> m q

type Position = Int

pos0 :: Position
pos0 = 0

type M a = P a
return = returnP
(>>=)  = bindP
fail   = failP

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | At Position Term

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
interp (At p t) e
 = resetP p (interp t e)

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

termP
 = Add (Con 1)
       (At 42
           (App (Con 2) (Con 3)))


main 
 = do
      putStrLn $ test term42
      putStrLn $ test termE
      putStrLn $ test termP
