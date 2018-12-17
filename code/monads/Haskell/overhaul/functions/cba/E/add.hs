add (Num i) (Num j) = return (Num (i+j))
add a b = throwErrorMsg ("should be numbers: "
                         ++ show a ++ ","
                         ++ show b)
