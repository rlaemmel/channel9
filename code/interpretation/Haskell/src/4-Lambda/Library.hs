#include "../3-NB/Library.hs"


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

