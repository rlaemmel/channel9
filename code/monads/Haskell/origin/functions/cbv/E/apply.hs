apply (Fun k) a = k a
apply f a = fail ("should be function: " ++ show f)
