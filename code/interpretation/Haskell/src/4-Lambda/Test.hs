-- Call-By-Value fixed-point operator

fix :: Expr
fix = Lambda "f" (Apply t t)
 where 
  t = Lambda "x" (Apply f (Lambda "y" (Apply (Apply x x) y)))
   where
    f = Var "f"
    x = Var "x"
    y = Var "y"


-- Addition

add :: Expr
add
 = Apply fix
   (Lambda "f"
   (Lambda "x"
   (Lambda "y"
     (Cond (IsZero (Var "x"))
       (Var "y")
       (Succ (Apply (Apply (Var "f") (Pred (Var "x"))) (Var "y")))))))


-- Multiplication

mult :: Expr
mult
 = Apply fix
   (Lambda "f"
   (Lambda "x"
   (Lambda "y"
     (Cond (IsZero (Var "x"))
       Zero
       (Apply (Apply add (Var "y")) (Apply (Apply (Var "f") (Pred (Var "x"))) (Var "y")))))))


-- Factorial 

fac :: Expr
fac 
 = Apply fix
   (Lambda "f"
   (Lambda "x"
     (Cond (IsZero (Var "x"))
        (Succ Zero)
        (Apply (Apply mult (Var "x")) (Apply (Var "f") (Pred (Var "x")))))))
