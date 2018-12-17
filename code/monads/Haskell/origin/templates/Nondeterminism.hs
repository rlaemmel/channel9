{-

The following code exercises Section 2.7 of the underlying article.
That section is concerned with non-deterministic choice.

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

#include "types/cbv/Value.hs"
 | Wrong

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"
  show Wrong = "<wrong>"

#include "functions/cbv/interp.hs"
interp Fail e
 = mzero
interp (Amb u v) e
 = interp u e `mplus` interp v e

#include "functions/cbv/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbv/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/L.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test Fail
      putStrLn $ test termL
