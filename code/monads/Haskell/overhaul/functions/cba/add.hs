add :: Monad m => Value m -> Value m -> m (Value m)
add (Num i) (Num j) = return (Num (i+j))
add a b = return Wrong
