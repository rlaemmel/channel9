-- We add core constructs of the lambda calculus.

#include "../3-NB/Syntax.hs"
 | Var String
 | Lambda String Expr
 | Apply Expr Expr

