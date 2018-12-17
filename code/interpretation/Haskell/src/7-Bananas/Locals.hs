data AlgebraExpr y
   = AlgebraExpr
 {
   zero   :: y
 , succ   :: y -> y
 , pred   :: y -> y
 , isZero :: y -> y
 , cond   :: y -> y -> y -> y
 , var    :: String -> y
 , lambda :: String -> y -> y
 , apply  :: y -> y -> y
 , letrec :: String -> y -> y -> y
 }

foldExpr :: AlgebraExpr y -> Expr -> y
foldExpr a = f 
 where
  f Zero = zero a
  f (Succ x) = succ a (f x)
  f (Pred x) = pred a (f x)
  f (IsZero x) = isZero a (f x)
  f (Cond x1 x2 x3) = cond a (f x1) (f x2) (f x3)
  f (Var n) = var a n
  f (Lambda n x) = lambda a n (f x)
  f (Apply x1 x2) = apply a (f x1) (f x2)
  f (Letrec n x1 x2) = letrec a n (f x1) (f x2)

closedTerm :: Expr -> Bool
closedTerm = null . foldExpr a
 where
  a :: AlgebraExpr (Set String)
  a =  AlgebraExpr {
     zero   = empty
   , succ   = id
   , pred   = id
   , isZero = id
   , cond   = \y1 y2 y3 -> unions [y1,y2,y3]
   , var    = singleton
   , lambda = delete
   , apply  = union
   , letrec = \n y1 y2 -> delete n (union y1 y2)
  }
