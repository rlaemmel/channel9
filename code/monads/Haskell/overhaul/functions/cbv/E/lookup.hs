lookup x [] = throwErrorMsg ("unbound variable: " ++ x)
lookup x ((y,b):e) = if x==y then return b else lookup x e
