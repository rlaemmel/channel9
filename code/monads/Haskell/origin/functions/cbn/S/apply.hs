apply :: Value -> M Value -> M Value
apply (Fun h) m = tick >>= \() -> h m
apply f m = return Wrong
