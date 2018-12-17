

{-

The following code exercises Section 3.2 of the underlying article.
That section is concerned with call with current continuation.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup, return, (>>=), fail)

-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b


callCC :: ((a -> M b) -> M a) -> M a

type K a = (a -> Answer) -> Answer

returnK :: a -> K a
returnK a = \c -> c a

bindK :: K a -> (a -> K b) -> K b
m `bindK` k = \c -> m (\a -> k a c)

callCCK :: ((a -> K b) -> K a) -> K a
callCCK h = \c -> let k a = \d -> c a
                  in  h k c

type Answer = Value

instance Show (M Value) where
  show m = show (m id)

type M a = K a
return   = returnK
(>>=)    = bindK
callCC   = callCCK

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Callcc Name Term

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
interp (Callcc x v) e
 = callCC (\k -> interp v ((x, Fun k):e))

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

termK 
 = Add (Con 1)
       (Callcc "k"
               (Add (Con 2)
                    (App (Var "k") (Con 4))))

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termK
