apply (Fun k) a = k a
apply f a = throwErrorMsg ("should be function: " ++ show f)
