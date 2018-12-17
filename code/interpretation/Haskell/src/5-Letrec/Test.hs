-- Addition

add :: Expr
add
 =  Lambda "x"
   (Lambda "y"
     (Cond (IsZero (Var "x"))
       (Var "y")
       (Succ (Apply (Apply (Var "add") (Pred (Var "x"))) (Var "y")))))


-- Multiplication

mult :: Expr
mult
 =  Lambda "x"
   (Lambda "y"
     (Cond (IsZero (Var "x"))
       Zero
       (Apply (Apply (Var "add") (Var "y")) (Apply (Apply (Var "mult") (Pred (Var "x"))) (Var "y")))))


-- Factorial 

fac :: Expr
fac 
 = Lambda "x"
     (Cond (IsZero (Var "x"))
       (Succ Zero)
       (Apply (Apply (Var "mult") (Var "x")) (Apply (Var "fac") (Pred (Var "x")))))


-- Composed system of functions and their application

program :: Expr -> Expr
program x
 = Letrec "add" add
    (Letrec "mult" mult
     (Letrec "fac" fac 
      x))
