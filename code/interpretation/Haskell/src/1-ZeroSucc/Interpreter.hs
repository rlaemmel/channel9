interpret :: Expr -> Value
interpret Zero = 0
interpret (Succ x) = (interpret x) + 1

