add (Num i) (Num j) = tick >>= \() -> return (Num (i+j))
add a b = return Wrong
