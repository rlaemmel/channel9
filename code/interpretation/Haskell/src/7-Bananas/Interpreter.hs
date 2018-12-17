-- We start allover because we want to go bananas.

interpret :: Expr -> Computation Value
interpret = foldExpr interpreterAlg

