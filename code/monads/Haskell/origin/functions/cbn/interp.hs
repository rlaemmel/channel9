interp :: Term -> Environment -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a -> interp v e >>= \b -> add a b
interp (Lam x v) e = return (Fun (\m -> interp v ((x,m):e)))
interp (App t u) e = interp t e >>= \f -> apply f (interp u e)
