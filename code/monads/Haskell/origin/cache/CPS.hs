

{-

The following code corresponds to Fig. 4 (Interpreter in CPS). We had
to gather all the missing features. One detail is that the Value type
also had to be updated so that the Fun constructor is typed in CPS as
well.

-}

import Prelude hiding (lookup)

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term

data Value =
   Wrong
 | Num Int
 | Fun (Value -> (Value -> Answer) -> Answer)

type Answer = Value

type Environment = [(Name, Value)]

instance Show Value where
  show (Num i) = show i
  show (Fun _) = "<function>"


interp :: Term -> Environment -> (Value -> Answer) -> Answer
interp (Var x) e = \c -> lookup x e c
interp (Con i) e = \c -> c (Num i)
interp (Add u v) e = \c -> interp u e (\a -> interp v e (\b -> add a b c))
interp (Lam x v) e = \c -> c (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = \c -> interp t e (\f -> interp u e (\a -> apply f a c))

lookup :: Name -> Environment -> (Value -> Answer) -> Answer
lookup _ [] = \c -> c Wrong
lookup x ((y,b):e) = \c -> if x==y then c b else lookup x e c

add :: Value -> Value -> (Value -> Answer) -> Answer
add (Num i) (Num j) = \c -> c (Num (i+j))
add _ _ = \c -> c Wrong

apply :: Value -> Value -> (Value -> Answer) -> Answer
apply (Fun k) a = \c -> k a c
apply _ _ = \c -> c Wrong


-- Test the interpreter

test :: Term -> String
test t = show (interp t [] id)

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

main 
 = do
      putStrLn $ test term42
