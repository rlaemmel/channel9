

{-

The following code provides execution counts for the interpreter by
means of using the state monad's type in the answer type of a
CPS-style interpreter. The underlying paper discusses this option in
Section 3.3.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (lookup, return, (>>=), fail)

-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b


tick :: M ()
fetch :: M State

{-

We use this monad specifically for the monad in the answer type of CPS.
See the discussion in Secion 3.3.

-}

-- Monad combinators to be defined for type constructor N
returnN :: a -> N a
bindN :: N a -> (a -> N b) -> N b

promoteK :: N a -> K a
promoteK m = \c -> m `bindN` c

tickN :: N ()
fetchN :: N State

type S a = State -> (a, State)

returnS :: a -> S a
returnS a = \s0 -> (a, s0)

bindS :: S a -> (a -> S b) -> S b
m `bindS` k = \s0 -> let (a,s1) = m s0 in k a s1

instance Show (S Value) where
  show m = let (a,s1) = m state0
           in "Value: " ++ show a ++ "; " ++ 
              "Count: " ++ show s1

tickS :: S ()
tickS = \s -> ((), s+1)

fetchS :: S State
fetchS = \s -> (s, s)

type State = Int

state0 :: State
state0 = 0

type N a = S a
returnN  = returnS
bindN  = bindS
tickN  = tickS
fetchN = fetchS

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
tick     = promoteK tickS
fetch    = promoteK fetchS

type Name = String

data Term =
   Var Name
 | Con Int
 | Add Term Term
 | Lam Name Term
 | App Term Term
 | Count

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
interp Count e
 = fetch >>= \i -> return (Num i)

lookup :: Name -> Environment -> M Value
lookup _ [] = return Wrong
lookup x ((y,b):e) = if x==y then return b else lookup x e

add (Num i) (Num j) = tick >>= \() -> return (Num (i+j))
add a b = return Wrong

apply (Fun k) a = tick >>= \() -> k a
apply f a = return Wrong

test :: Term -> String
test t = show (interp t [])

term42
 = App (Lam "x"
            (Add (Var "x") (Var "x")))
       (Add (Con 10) (Con 11))

termS
 = Add (Add (Con 1) (Con 2)) Count

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termS
