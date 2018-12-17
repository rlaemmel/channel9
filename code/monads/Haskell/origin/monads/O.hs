type O a = (a, String)

returnO :: a -> O a
returnO a = (a, "")

bindO :: O a -> (a -> O b) -> O b
m `bindO` k = let (a,r) = m
                  (b,s) = k a
              in  (b,r++s)

{-

We cannot submit a Show instance so that we would reproduce
the precise output of the original paper. The trouble is that
we would need an overlapping instance, but GHC does not 
allow it for Prelude classes these days.

-}

tellO :: Value -> O ()
tellO a = ((), show a ++ "; ")
