type L a = [a]

returnL :: a -> L a
returnL a = [a]

bindL :: L a -> (a -> L b) -> L b
m `bindL` k = [ b | a <- m, b <- k a ]

mzeroL :: L a
mzeroL = []

mplusL :: L a -> L a -> L a
l `mplusL` m = l ++ m

