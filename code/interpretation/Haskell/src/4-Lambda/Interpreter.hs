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

