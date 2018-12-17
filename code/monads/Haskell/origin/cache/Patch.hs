

{-

This code is not part of the underlying paper, but it uses the state
scenario from the paper. We simply demonstrate what it takes to patch
the baseline interpreter so that it can deal with state---if we do not
leverage the monad abstraction.

-}

import Prelude hiding (lookup)

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Count

data Value =
   Wrong
 | Num Int
 | Fun (Value -> State -> (Value, State))

instance Show Value where
  show (Num i) = show i
  show (Fun _) = "<function>"


type Environment = [(Name, Value)]

type State = Int

interp :: Term -> Environment -> State -> (Value, State)
interp (Var x) e s = (lookup x e, s)
interp (Con i) e s = (Num i, s)
interp (Add u v) e s0 = let (v1,s1) = interp u e s0
                            (v2,s2) = interp v e s1
                        in add v1 v2 s2
interp (Lam x v) e s = (Fun (\a -> interp v ((x,a):e)), s)
interp (App t u) e s0 = let (v1,s1) = interp t e s0
                            (v2,s2) = interp u e s1
                        in apply v1 v2 s2
interp Count e s = (Num s,s)

lookup :: Name -> Environment -> Value
lookup _ [] = Wrong
lookup x ((y,b):e) = if x==y then b else lookup x e

add :: Value -> Value -> State -> (Value, State)
add (Num i) (Num j) s = (Num (i+j), s+1)
add _ _ s = (Wrong, s)

apply :: Value -> Value -> State -> (Value, State)
apply (Fun k) a s0 = let (v,s1) = k a s0 in (v,s1+1)
apply _ _ s = (Wrong, s)


-- Test the interpreter

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termS
 = Add (Add (Con 1) (Con 2)) Count

main 
 = do
      print $ interp term42 [] 0
      print $ interp termS [] 0
