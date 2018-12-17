

{-

The following code deals with the state monad (a la Section 2.5) in a
CBN monadic-style interpreter. This variation is suggested in Section
2.9.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup, return, (>>=), fail)

-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b


tick :: M ()
fetch :: M State

type S a = State -> (a, State)

returnS :: a -> S a
returnS a = \s0 -> (a, s0)

bindS :: S a -> (a -> S b) -> S b
m `bindS` k = \s0 -> let (a,s1) = m s0 in k a s1

instance Show (S Value) where
  show m = let (a,s1) = m state0
           in "Value: " ++ show a ++ "; " ++ 
              "Count: " ++ show s1

tickS :: S ()
tickS = \s -> ((), s+1)

fetchS :: S State
fetchS = \s -> (s, s)

type State = Int

state0 :: State
state0 = 0

type M a = S a
return = returnS
(>>=)  = bindS
tick   = tickS
fetch  = fetchS

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Count

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
interp Count e
 = fetch >>= \i -> return (Num i)

lookup :: Name -> Environment -> M Value
lookup _ [] = return Wrong
lookup x ((y,n):e) = if x==y then n else lookup x e

add (Num i) (Num j) = tick >>= \() -> return (Num (i+j))
add a b = return Wrong

apply :: Value -> M Value -> M Value
apply (Fun h) m = tick >>= \() -> h m
apply f m = return Wrong

test :: Term -> String
test t = show (interp t [])

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termS
 = Add (Add (Con 1) (Con 2)) Count

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termS
