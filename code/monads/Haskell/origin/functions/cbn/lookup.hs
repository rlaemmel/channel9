lookup :: Name -> Environment -> M Value
lookup _ [] = return Wrong
lookup x ((y,n):e) = if x==y then n else lookup x e
