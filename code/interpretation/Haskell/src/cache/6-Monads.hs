

import Prelude hiding (succ, pred, null)





import Control.Monad.Identity
import Control.Monad.Maybe
import Control.Monad.Reader

-- We add a recursive let to the applicative lambda calculus.

-- We add core constructs of the lambda calculus.

-- We add operations for the Booleans.

-- We add the predecessor operation.

-- Peano-like expression forms for natural numbers

data Expr
 = Zero
 | Succ Expr

 | Pred Expr

 | IsZero Expr
 | Cond Expr Expr Expr
 | Var String
 | Lambda String Expr
 | Apply Expr Expr

 | Letrec String Expr Expr


-- Natural numbers as Ints
-- Riddle: Define a designated algebraic datatype!

type Nat = Int





-- Environments

type Env = String -> Maybe Value




-- MTL-based derivation of semantic domain

type Computation = ReaderT Env (MaybeT Identity)

-- We add functions as a form of values.

-- Interpretation returns natural numbers and Booleans.

data Value
 = InNat Nat
 | InBool Bool
 | InFun (Value -> Maybe Value)


-- We start allover in monadic style.

interpret :: Expr -> Computation Value

interpret Zero
 = return (InNat 0)

interpret (Succ x)
 = do
      r <- interpret x
      n <- try (outNat r)
      return (InNat (n+1))

interpret (Pred x)
 = do
      r <- interpret x
      n <- try (outNat r)
      guard (n>0)
      return (InNat (n-1))

interpret (IsZero x)
 = do
      r <- interpret x
      n <- try (outNat r)
      return (InBool (n==0))

interpret (Cond xc xt xe)
 = do
      r <- interpret xc
      b <- try (outBool r)
      if b
        then interpret xt
        else interpret xe

interpret (Var n)
 = do 
      e <- ask
      try (e n)

interpret (Lambda n x)
 = do
      e <- ask
      return (InFun (\r -> run (interpret x)
                               (modify e n (Just r))))

interpret (Apply xf xa)
 = do
      f <- interpret xf
      a <- interpret xa
      g <- try (outFun f)
      try (g a)

interpret (Letrec n x1 x2)
 = do
      local (\e -> fix (modify e n . run (interpret x1)))
            (interpret x2)
-- We don't use an algebra for interpretation in this version.

-- Partial function application

infixr 0 $$
($$) :: (a -> Maybe b) -> Maybe a -> Maybe b
($$) = maybe Nothing



-- Auxiliary functions for domain Value

outNat :: Value -> Maybe Nat
outNat (InNat n) = Just n
outNat _ = Nothing

outBool :: Value -> Maybe Bool
outBool (InBool b) = Just b
outBool _ = Nothing



-- Auxiliary functions for domain Value

outFun :: Value -> Maybe (Value -> Maybe Value)
outFun (InFun f) = Just f
outFun _ = Nothing

instance Show Value
 where
  show (InNat n)  = show n
  show (InBool b) = show b
  show (InFun _)  = "<function>"


-- Point-wise function modification

modify :: Eq x => (x -> y) -> x -> y -> x -> y
modify f x y x' = if x==x' then y else f x'


-- Convert Int into expression

fromInt :: Int -> Expr
fromInt 0 = Zero
fromInt n = Succ (fromInt (n-1))




-- Get a Maybe out of a computation

run :: Computation t -> Env -> Maybe t
run t e = runIdentity (runMaybeT (runReaderT t e))


-- Turn a plain Maybe into a computation

try :: Maybe a -> Computation a
try = maybe (fail undefined) return

-- There are no auxiliary definitions that are local to this version.

-- Addition

add :: Expr
add
 =  Lambda "x"
   (Lambda "y"
     (Cond (IsZero (Var "x"))
       (Var "y")
       (Succ (Apply (Apply (Var "add") (Pred (Var "x"))) (Var "y")))))


-- Multiplication

mult :: Expr
mult
 =  Lambda "x"
   (Lambda "y"
     (Cond (IsZero (Var "x"))
       Zero
       (Apply (Apply (Var "add") (Var "y")) (Apply (Apply (Var "mult") (Pred (Var "x"))) (Var "y")))))


-- Factorial 

fac :: Expr
fac 
 = Lambda "x"
     (Cond (IsZero (Var "x"))
       (Succ Zero)
       (Apply (Apply (Var "mult") (Var "x")) (Apply (Var "fac") (Pred (Var "x")))))


-- Composed system of functions and their application

program :: Expr -> Expr
program x
 = Letrec "add" add
    (Letrec "mult" mult
     (Letrec "fac" fac 
      x))

main :: IO ()
main
 = do 
      let e = const Nothing
      let x5 = fromInt 5
      print $ run (interpret (program (Apply (Var "fac") x5))) e
