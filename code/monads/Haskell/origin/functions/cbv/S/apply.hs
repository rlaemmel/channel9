apply (Fun k) a = tick >>= \() -> k a
apply f a = return Wrong
