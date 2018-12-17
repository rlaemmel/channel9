{-

(C) 2010, Ralf Laemmel

We compare two versions of a lambda interpreter. The "classic" version
uses general recursion, but inspection suggests that compositional
style is used. The "algebraic" version uses large bananas. In fact, we
also another example on both sides: a free variable analysis, which
can nicely benefit from a generic fold algebra for monoidal reduction.

Ralf Laemmel and Joost Visser and Jan Kort
Dealing with Large Bananas
Workshop on Generic Programming (WGP'00), 2000.

-}

module Large where

import Lambda.Domains
import qualified Lambda.Classic (eval, fv)
import qualified Lambda.Algebraic (eval, fv, fv')
import Data.Map (Map, empty)


-- Test the interpreter

-- Call-By-Value fixed-point operator

fix :: Term
fix = Lambda "f" (Apply t t)
 where 
  t = Lambda "x" (Apply f (Lambda "y" (Apply (Apply x x) y)))
   where
    f = Var "f"
    x = Var "x"
    y = Var "y"


-- Addition

add :: Term
add
 = Apply fix
   (Lambda "f"
   (Lambda "x"
   (Lambda "y"
     (Cond (IsZero (Var "x"))
       (Var "y")
       (Succ (Apply (Apply (Var "f") (Pred (Var "x"))) (Var "y")))))))


-- Multiplication

mult :: Term
mult
 = Apply fix
   (Lambda "f"
   (Lambda "x"
   (Lambda "y"
     (Cond (IsZero (Var "x"))
       Zero
       (Apply (Apply add (Var "y")) (Apply (Apply (Var "f") (Pred (Var "x"))) (Var "y")))))))


-- Factorial 

fac :: Term
fac 
 = Apply fix
   (Lambda "f"
   (Lambda "x"
     (Cond (IsZero (Var "x"))
        (Succ Zero)
        (Apply (Apply mult (Var "x")) (Apply (Var "f") (Pred (Var "x")))))))


-- Convert natural number into expression

fromInt :: Int -> Term
fromInt 0 = Zero
fromInt n = Succ (fromInt (n-1))


main 
 = do
      let e = empty
      let x5 = fromInt 5
      print $ Lambda.Classic.eval (Apply fac x5) e
      print $ Lambda.Algebraic.eval (Apply fac x5) e
      let free = Lambda "x" (Apply (Var "x") (Var "y"))
      print $ Lambda.Classic.fv free
      print $ Lambda.Algebraic.fv free
      print $ Lambda.Algebraic.fv' free
