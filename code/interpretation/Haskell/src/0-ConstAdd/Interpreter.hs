evaluate :: Expr -> Int
evaluate (Const i) = i
evaluate (Add x1 x2) = evaluate x1 + evaluate x2

