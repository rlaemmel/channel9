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

