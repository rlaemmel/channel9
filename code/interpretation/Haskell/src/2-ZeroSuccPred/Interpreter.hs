interpret :: Expr -> Maybe Value
interpret Zero     = Just 0
interpret (Succ x) = (Just . (+1)) $$ interpret x
interpret (Pred x) = pred $$ interpret x
 where
  pred n | n > 0     = Just (n-1)
         | otherwise = Nothing
  
