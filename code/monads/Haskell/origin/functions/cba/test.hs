test :: Term -> String
test t = show (interp t [])
