

{-

We instantiate CBV monadic style with the error monad.
We use the existing library implementation based on Either.
We also revise some meanings to produce useful error messages.

-}

import Prelude hiding (lookup)
import Control.Monad.Error

throwErrorMsg :: (Error e, MonadError e m) => String -> m a
throwErrorMsg = fail . strMsg

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term

data Value m =
   Wrong
 | Num Int
 | Fun (Value m -> m (Value m))

type Environment m = [(Name, Value m)]

instance Monad m => Show (Value m) where
  show Wrong = "<wrong>"
  show (Num i) = show i
  show (Fun _) = "<function>"

interp :: (Error e, MonadError e m) => Term -> Environment m -> m (Value m)
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a

lookup x [] = throwErrorMsg ("unbound variable: " ++ x)
lookup x ((y,b):e) = if x==y then return b else lookup x e

add (Num i) (Num j) = return (Num (i+j))
add a b = throwErrorMsg ("should be numbers: "
                         ++ show a ++ ","
                         ++ show b)

apply (Fun k) a = k a
apply f a = throwErrorMsg ("should be function: " ++ show f)

test :: Term -> Either String (Value (Either String))
test t = interp t []

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termE
 = App (Con 1) (Con 2)

main 
 = do
      putStrLn $ prettyPrint (test term42)
      putStrLn $ prettyPrint (test termE)
 where
  prettyPrint (Left s)  = "<error: " ++ s ++ ">"
  prettyPrint (Right a) = show a
