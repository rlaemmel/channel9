

{-

This is Fig. 3 of the underlying paper.

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
 | Fun (M Value -> M Value)

type Environment = [(Name, M Value)]

instance Show Value where
  show (Num i) = show i
  show (Fun _) = "<function>"


interp :: Term -> Environment -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\m -> interp v ((x,m):e)))
interp (App t u) e = interp t e >>= \f -> apply f (interp u e)

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
