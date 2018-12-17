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
