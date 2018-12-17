

{-

The following code exercises Section 2.2 of the underlying article.
That section is concerned with the use of the identity monad for the
instantiation of the monadic-style interpreter. We do *not* import
Control.Monad.Identity. Instead, we follow the original paper closely
For the identity monad, see here:

http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/src/Control-Monad-Identity.html

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup, return, (>>=), fail)

-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b


type I a = a

returnI :: a -> I a
returnI a = a

bindI :: I a -> (a -> I b) -> I b
a `bindI` k = k a


type M a = I a
return = returnI
(>>=) = bindI



{-

This is Fig. 1 of the underlying paper.

-}

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

termE
 = App (Con 1) (Con 2)

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termE
