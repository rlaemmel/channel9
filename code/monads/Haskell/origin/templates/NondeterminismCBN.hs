{-

The following code deals with nondeterminism (a la Section 2.7) in a
CBN monadic-style interpreter. This variation is suggested in Section
2.9.

-}

#include "Imports.hs"

#include "monads/M.hs"

mzero :: M a
mplus :: M a -> M a -> M a

#include "monads/L.hs"

type M a = L a
return = returnL
(>>=)  = bindL
mzero  = mzeroL
mplus  = mplusL

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"
 | Fail
 | Amb Term Term

#include "types/cbn/Value.hs"
 | Wrong

#include "types/cbn/Environment.hs"

#include "functions/cba/showval.hs"
  show Wrong = "<wrong>"

#include "functions/cbn/interp.hs"
interp Fail e
 = mzero
interp (Amb u v) e
 = interp u e `mplus` interp v e

#include "functions/cbn/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbn/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/L.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test Fail
      putStrLn $ test termL
