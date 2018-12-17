#include "../2-ZeroSuccPred/Library.hs"


-- Auxiliary functions for domain Value

outNat :: Value -> Maybe Nat
outNat (InNat n) = Just n
outNat _ = Nothing

outBool :: Value -> Maybe Bool
outBool (InBool b) = Just b
outBool _ = Nothing

