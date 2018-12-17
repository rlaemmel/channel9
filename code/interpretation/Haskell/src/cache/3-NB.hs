

import Prelude hiding (succ, pred, null)


-- We add operations for the Booleans.

-- We add the predecessor operation.

-- Peano-like expression forms for natural numbers

data Expr
 = Zero
 | Succ Expr

 | Pred Expr

 | IsZero Expr
 | Cond Expr Expr Expr
-- Natural numbers as Ints
-- Riddle: Define a designated algebraic datatype!

type Nat = Int



-- Interpretation returns natural numbers and Booleans.

data Value
 = InNat Nat
 | InBool Bool
interpret :: Expr -> Maybe Value
interpret Zero       = zero 
interpret (Succ x)   = succ $$ interpret x
interpret (Pred x)   = pred $$ interpret x
interpret (IsZero x) = isZero $$ interpret x
interpret (Cond xc xt xe) = cond yc yt ye
 where
  yc = interpret xc
  yt = interpret xt
  ye = interpret xe

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

instance Show Value
 where
  show (InNat n)  = show n
  show (InBool b) = show b

-- All testing-related code, if any, resides in the main file.

main :: IO ()
main
 = do 
      let x0 = Zero
      let x1 = Succ x0
      let x2 = IsZero x1
      let x3 = Cond x2 x0 x1
      print $ interpret x3
