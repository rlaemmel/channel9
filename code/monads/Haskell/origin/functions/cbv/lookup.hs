lookup :: Name -> Environment -> M Value
lookup _ [] = return Wrong
lookup x ((y,b):e) = if x==y then return b else lookup x e
