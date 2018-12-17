apply :: Value -> M Value -> M Value
apply (Fun h) m = h m
apply f m = return Wrong
