{-

We use this monad specifically for the monad in the answer type of CPS.
See the discussion in Secion 3.3.

-}

-- Monad combinators to be defined for type constructor N
returnN :: a -> N a
bindN :: N a -> (a -> N b) -> N b

promoteK :: N a -> K a
promoteK m = \c -> m `bindN` c
