add (Num i) (Num j) = return (Num (i+j))
add a b = fail ("should be numbers: "
                ++ show a ++ ","
                ++ show b)
