

{-

We instantiate CBV monadic style with the state monad.
We use the state to maintain and query the reduction count.

-}

{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (lookup)
import Control.Monad.State

tick :: MonadState Int m => m ()
tick = get >>= put . (+1)

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Count

data Value m =
   Wrong
 | Num Int
 | Fun (Value m -> m (Value m))

type Environment m = [(Name, Value m)]

instance Monad m => Show (Value m) where
  show Wrong = "<wrong>"
  show (Num i) = show i
  show (Fun _) = "<function>"

interp :: MonadState Int m => Term -> Environment m -> m (Value m)
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a
interp Count e
 = get >>= \i -> return (Num i)

lookup :: Monad m => Name -> Environment m -> m (Value m)
lookup _ [] = return Wrong
lookup x ((y,b):e) = if x==y then return b else lookup x e

add (Num i) (Num j) = tick >> return (Num (i+j))
add a b = return Wrong

apply (Fun k) a = tick >> k a
apply f a = return Wrong

test :: Term -> (Value (State Int), Int)
test t = runState (interp t []) state0
 where
  state0 = 0

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termS
 = Add (Add (Con 1) (Con 2)) Count

main 
 = do
      putStrLn $ prettyPrint (test term42)
      putStrLn $ prettyPrint (test termS)
 where
  prettyPrint (a,s1)
   = "Value: " ++ show a ++ "; " ++ 
     "Count: " ++ show s1
