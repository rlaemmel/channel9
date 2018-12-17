type P a = Position -> E a

returnP :: a -> P a
returnP a = \p -> returnE a

bindP :: P a -> (a -> P b) -> P b
m `bindP` k = \p -> m p `bindE` \x -> k x p

instance Show (P Value) where
  show f = show (f pos0)

failP :: String -> P a
failP s = \p -> failE (show p ++ ": " ++ s)

resetP :: Position -> P x -> P x
resetP q m = \p -> m q
