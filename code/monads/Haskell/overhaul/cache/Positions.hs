

{-

We advance the earlier effort regarding error messages.
That is, we add positions to terms so that error messages can refer to them.
In this manner, we need to stack together a monad.

-}

import Prelude hiding (lookup)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

type Position = Int

type M = ReaderT Position (ErrorT String Identity)

throwErrorMsg :: String -> M a
throwErrorMsg s
 = do
      p <- ask
      fail (show p ++ ": " ++ s)

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | At Position Term

data Value m =
   Wrong
 | Num Int
 | Fun (Value m -> m (Value m))

type Environment m = [(Name, Value m)]

instance Monad m => Show (Value m) where
  show Wrong = "<wrong>"
  show (Num i) = show i
  show (Fun _) = "<function>"

interp :: Term -> Environment M -> M (Value M)
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a
interp (At p t) e
 = local (const p) (interp t e)

lookup x [] = throwErrorMsg ("unbound variable: " ++ x)
lookup x ((y,b):e) = if x==y then return b else lookup x e

add (Num i) (Num j) = return (Num (i+j))
add a b = throwErrorMsg ("should be numbers: "
                         ++ show a ++ ","
                         ++ show b)

apply (Fun k) a = k a
apply f a = throwErrorMsg ("should be function: " ++ show f)

test :: Term -> Either String (Value M)
test t = runIdentity
       $ runErrorT 
       $ runReaderT (interp t []) pos0
 where
  pos0 :: Position
  pos0 = 0

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termE
 = App (Con 1) (Con 2)

termP
 = Add (Con 1)
       (At 42
           (App (Con 2) (Con 3)))


main 
 = do
      putStrLn $ prettyPrint (test term42)
      putStrLn $ prettyPrint (test termE)
      putStrLn $ prettyPrint (test termP)
 where
  prettyPrint (Left s)  = "<error: " ++ s ++ ">"
  prettyPrint (Right a) = show a
