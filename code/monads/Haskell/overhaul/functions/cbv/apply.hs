apply :: Monad m => Value m -> Value m -> m (Value m)
apply (Fun k) a = k a
apply f a = return Wrong
