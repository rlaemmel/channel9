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

