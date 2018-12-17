-- Monad combinators to be defined for type constructor M
return :: a -> M a
(>>=) :: M a -> (a -> M b) -> M b

