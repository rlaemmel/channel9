

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
