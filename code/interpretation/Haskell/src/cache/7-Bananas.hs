

import Prelude hiding (succ, pred, null)





import Control.Monad.Identity
import Control.Monad.Maybe
import Control.Monad.Reader


import Data.Set

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



-- We start allover because we want to go bananas.

interpret :: Expr -> Computation Value
interpret = foldExpr interpreterAlg

-- We use bananas for the algebra.

interpreterAlg :: AlgebraExpr (Computation Value)
interpreterAlg
 = AlgebraExpr 
 {
   zero
    = return (InNat 0)

 , succ = \y -> 
     do
        r <- y
        n <- try (outNat r)
        return (InNat (n+1))

 , pred = \y -> 
     do
        r <- y
        n <- try (outNat r)
        guard (n>0)
        return (InNat (n-1))

 , isZero = \y ->
     do
        r <- y
        n <- try (outNat r)
        return (InBool (n==0))

 , cond = \yc yt ye ->
     do
        r <- yc
        b <- try (outBool r)
        if b then yt else ye

 , var = \n ->
     do
        e <- ask
        try (e n)

 , lambda = \n y ->
     do
        e <- ask
        return (InFun (\r -> run y (modify e n (Just r))))

 , apply = \yf ya ->
     do
        f <- yf
        a <- ya
        g <- try (outFun f)
        try (g a)

 , letrec = \n y1 y2 ->
     do
        local (\e -> fix (modify e n . run y1)) y2

 }

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


data AlgebraExpr y
   = AlgebraExpr
 {
   zero   :: y
 , succ   :: y -> y
 , pred   :: y -> y
 , isZero :: y -> y
 , cond   :: y -> y -> y -> y
 , var    :: String -> y
 , lambda :: String -> y -> y
 , apply  :: y -> y -> y
 , letrec :: String -> y -> y -> y
 }

foldExpr :: AlgebraExpr y -> Expr -> y
foldExpr a = f 
 where
  f Zero = zero a
  f (Succ x) = succ a (f x)
  f (Pred x) = pred a (f x)
  f (IsZero x) = isZero a (f x)
  f (Cond x1 x2 x3) = cond a (f x1) (f x2) (f x3)
  f (Var n) = var a n
  f (Lambda n x) = lambda a n (f x)
  f (Apply x1 x2) = apply a (f x1) (f x2)
  f (Letrec n x1 x2) = letrec a n (f x1) (f x2)

closedTerm :: Expr -> Bool
closedTerm = null . foldExpr a
 where
  a :: AlgebraExpr (Set String)
  a =  AlgebraExpr {
     zero   = empty
   , succ   = id
   , pred   = id
   , isZero = id
   , cond   = \y1 y2 y3 -> unions [y1,y2,y3]
   , var    = singleton
   , lambda = delete
   , apply  = union
   , letrec = \n y1 y2 -> delete n (union y1 y2)
  }
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

-- Add some test cases

      let closed1 = Lambda "x" Zero
      let closed2 = Lambda "x" (Var "x")
      let open1   = Lambda "x" (Var "y")
      let open2   = Lambda "x" (Succ (Var "y"))
      print $ closedTerm closed1
      print $ closedTerm closed2
      print $ closedTerm open1
      print $ closedTerm open2

