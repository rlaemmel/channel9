

import Prelude hiding (succ, pred, null)




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


-- We add functions as a form of values.

-- Interpretation returns natural numbers and Booleans.

data Value
 = InNat Nat
 | InBool Bool
 | InFun (Value -> Maybe Value)

-- We start allover because the type of the function changes.

interpret :: Expr -> Env -> Maybe Value

-- The following equations are as in NB but with environment passing.

interpret Zero       _ = zero
interpret (Succ x)   e = succ $$ interpret x e
interpret (Pred x)   e = pred $$ interpret x e
interpret (IsZero x) e = isZero $$ interpret x e
interpret (Cond xc xt xe) e = cond yc yt ye
 where
  yc = interpret xc e
  yt = interpret xt e
  ye = interpret xe e

-- The following equations cover the new constructs for the lambda calculus.

interpret (Var n) e
 = var e n

interpret (Lambda n x) e
 = lambda e n (interpret x)

interpret (Apply xf xa) e
 = apply (interpret xf e) (interpret xa e)


interpret (Letrec n x1 x2) e
 = interpret x2 (fix (modify e n . interpret x1))
zero :: Maybe Value
zero = Just (InNat 0)

succ :: Value -> Maybe Value
succ = ($$) (Just . InNat . (+1)) . outNat

pred :: Value -> Maybe Value
pred = ($$) pred' . outNat
 where
  pred' n | n > 0     = Just (InNat (n-1))
          | otherwise = Nothing

isZero :: Value -> Maybe Value
isZero = ($$) (Just . InBool . (==0)) . outNat

cond :: Maybe Value -> Maybe Value -> Maybe Value -> Maybe Value
cond rc rt re = cond' $$ (outBool $$ rc)
 where
  cond' b = if b then rt else re

var :: Env -> String -> Maybe Value
var = ($)

lambda :: Env -> String -> (Env -> Maybe Value) -> Maybe Value
lambda e n f = Just (InFun (\r -> f (modify e n (Just r))))

apply :: Maybe Value -> Maybe Value -> Maybe Value
apply f a 
 = (\f' -> 
   (\a' -> (flip ($) a') 
     $$ outFun f') 
     $$ a) 
     $$ f

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


-- Fixed-point combinator

fix :: (x -> x) -> x
fix f = f (fix f)

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
      print $ interpret (program (Apply (Var "fac") x5)) e

