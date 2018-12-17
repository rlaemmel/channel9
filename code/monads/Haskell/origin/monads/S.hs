type S a = State -> (a, State)

returnS :: a -> S a
returnS a = \s0 -> (a, s0)

bindS :: S a -> (a -> S b) -> S b
m `bindS` k = \s0 -> let (a,s1) = m s0 in k a s1

instance Show (S Value) where
  show m = let (a,s1) = m state0
           in "Value: " ++ show a ++ "; " ++ 
              "Count: " ++ show s1

tickS :: S ()
tickS = \s -> ((), s+1)

fetchS :: S State
fetchS = \s -> (s, s)
