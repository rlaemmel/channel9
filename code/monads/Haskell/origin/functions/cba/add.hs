add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num (i+j))
add a b = return Wrong
