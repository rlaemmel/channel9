

{-

The following code provides error messages for the interpreter by
means of using the error monad's type in the answer type of a
CPS-style interpreter. The underlying paper discusses this option in
Section 3.3.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup, return, (>>=), fail)

-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b


fail :: String -> M a

{-

We use this monad specifically for the monad in the answer type of CPS.
See the discussion in Secion 3.3.

-}

-- Monad combinators to be defined for type constructor N
returnN :: a -> N a
bindN :: N a -> (a -> N b) -> N b

promoteK :: N a -> K a
promoteK m = \c -> m `bindN` c

failN :: String -> N a

data E a
 = Suc a
 | Err String

instance Show (E Value) where
  show (Suc a) = show a
  show (Err s) = "<error: " ++ s ++ ">"

returnE :: a -> E a
returnE a = Suc a

bindE :: E a -> (a -> E b) -> E b
(Suc a) `bindE` k = k a
(Err s) `bindE` k = Err s

failE :: String -> E a
failE s = Err s

type N a = E a
returnN  = returnE
bindN    = bindE
failN    = failE

type K a = (a -> Answer) -> Answer

returnK :: a -> K a
returnK a = \c -> c a

bindK :: K a -> (a -> K b) -> K b
m `bindK` k = \c -> m (\a -> k a c)

callCCK :: ((a -> K b) -> K a) -> K a
callCCK h = \c -> let k a = \d -> c a
                  in  h k c

type Answer = N Value

instance Show (K Value) where
  show m = show (m returnN)

type M a = K a
return   = returnK
(>>=)    = bindK
fail s   = promoteK (failE s)

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

type Environment = [(Name, Value)]

instance Show Value where
  show (Num i) = show i
  show (Fun _) = "<function>"


interp :: Term -> Environment -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a

lookup x [] = fail ("unbound variable: " ++ x) 
lookup x ((y,b):e) = if x==y then return b else lookup x e

add (Num i) (Num j) = return (Num (i+j))
add a b = fail ("should be numbers: "
                ++ show a ++ ","
                ++ show b)

apply (Fun k) a = k a
apply f a = fail ("should be function: " ++ show f)

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
