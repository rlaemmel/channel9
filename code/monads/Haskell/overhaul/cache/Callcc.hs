

{-

We instantiate CBV monadic style with the list monad.
In this manner, we add "call with current continuation" to the language.

-}

import Prelude hiding (lookup)
import Control.Monad.Cont

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Callcc Name Term

data Value m =
   Wrong
 | Num Int
 | Fun (Value m -> m (Value m))

type Environment m = [(Name, Value m)]

instance Monad m => Show (Value m) where
  show Wrong = "<wrong>"
  show (Num i) = show i
  show (Fun _) = "<function>"

interp :: MonadCont m => Term -> Environment m -> m (Value m)
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a
interp (Callcc x v) e
 = callCC (\k -> interp v ((x, Fun k):e))

lookup :: Monad m => Name -> Environment m -> m (Value m)
lookup _ [] = return Wrong
lookup x ((y,b):e) = if x==y then return b else lookup x e

add :: Monad m => Value m -> Value m -> m (Value m)
add (Num i) (Num j) = return (Num (i+j))
add a b = return Wrong

apply :: Monad m => Value m -> Value m -> m (Value m)
apply (Fun k) a = k a
apply f a = return Wrong

{-

We use "Answer = String"; we cannot use "Answer = Value" because Value
is parameterized in the monad and the monad Cont would eventually get
instantiated with Value, which triggers the occurs check. We could
only avoid this problem if we were rewriting all types and function
signatures to hardwire Cont into Value. Wonderful extensibility!

-}

test :: Term -> (Value (Cont r) -> r) -> r
test t = runCont (interp t [])

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
      putStrLn $ test term42 show
      putStrLn $ test termK show
