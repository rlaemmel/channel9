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
