apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a = return Wrong
