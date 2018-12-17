type I a = a

returnI :: a -> I a
returnI a = a

bindI :: I a -> (a -> I b) -> I b
a `bindI` k = k a

