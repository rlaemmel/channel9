data E a
 = Suc a
 | Err String

instance Show (E Value) where
  show (Suc a) = show a
  show (Err s) = "<error: " ++ s ++ ">"

returnE :: a -> E a
returnE a = Suc a

bindE :: E a -> (a -> E b) -> E b
(Suc a) `bindE` k = k a
(Err s) `bindE` k = Err s

failE :: String -> E a
failE s = Err s
