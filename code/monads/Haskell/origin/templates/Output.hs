{-

The following code exercises Section 2.6 of the underlying article.
That section is concerned with lazy output.

-}

#include "Imports.hs"

#include "monads/M.hs"

tell :: Value -> M ()

#include "monads/O.hs"

type M a = O a
return = returnO
(>>=)  = bindO
tell   = tellO

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"
 | Out Term

#include "types/cbv/Value.hs"
 | Wrong

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"
  show Wrong = "<wrong>"

#include "functions/cbv/interp.hs"
interp (Out u) e
 = interp u e >>= \a ->
   tell a     >>= \() -> 
   return a

#include "functions/cbv/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbv/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/O.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termO
