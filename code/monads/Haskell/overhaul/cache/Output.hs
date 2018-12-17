

{-

We instantiate CBV monadic style with the writer monad.
We use the writer for an output construct in the interpreted language.

-}

{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (lookup)
import Control.Monad.Writer.Lazy

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Out Term

data Value m =
   Wrong
 | Num Int
 | Fun (Value m -> m (Value m))

type Environment m = [(Name, Value m)]

instance Monad m => Show (Value m) where
  show Wrong = "<wrong>"
  show (Num i) = show i
  show (Fun _) = "<function>"

interp :: MonadWriter String m => Term -> Environment m -> m (Value m)
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a
interp (Out u) e
 = do
      a <- interp u e
      tell (show a ++ "; ")
      return a

lookup :: Monad m => Name -> Environment m -> m (Value m)
lookup _ [] = return Wrong
lookup x ((y,b):e) = if x==y then return b else lookup x e

add :: Monad m => Value m -> Value m -> m (Value m)
add (Num i) (Num j) = return (Num (i+j))
add a b = return Wrong

apply :: Monad m => Value m -> Value m -> m (Value m)
apply (Fun k) a = k a
apply f a = return Wrong

test :: Term -> (Value (Writer String), String)
test t = runWriter (interp t [])

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termO
 = Add (Out (Con 41)) (Out (Con 1))


main 
 = do
      putStrLn $ show (test term42)
      putStrLn $ show (test termO)
