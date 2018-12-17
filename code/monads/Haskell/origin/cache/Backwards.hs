

{-

The following code exercises Section 2.8 of the underlying article.
That section is concerned with propagating state backwards.

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
(>>=)  = bindS'
tick   = tickS
fetch  = fetchS

bindS' :: S a -> (a -> S b) -> S b
m `bindS'` k = \s2 -> let (a,s0) = m s1
                          (b,s1) = k a s2
                      in  (b,s0)

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
interp Count e
 = fetch >>= \i -> return (Num i)

lookup :: Name -> Environment -> M Value
lookup _ [] = return Wrong
lookup x ((y,b):e) = if x==y then return b else lookup x e

add (Num i) (Num j) = tick >>= \() -> return (Num (i+j))
add a b = return Wrong

apply (Fun k) a = tick >>= \() -> k a
apply f a = return Wrong

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
