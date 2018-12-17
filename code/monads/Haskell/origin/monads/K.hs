type K a = (a -> Answer) -> Answer

returnK :: a -> K a
returnK a = \c -> c a

bindK :: K a -> (a -> K b) -> K b
m `bindK` k = \c -> m (\a -> k a c)

callCCK :: ((a -> K b) -> K a) -> K a
callCCK h = \c -> let k a = \d -> c a
                  in  h k c
