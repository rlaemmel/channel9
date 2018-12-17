{-

The following code exercises Section 2.3 of the underlying article.
That section is concerned with the use a kind of error monad for the
purpose of producing useful error messages along interpretation.

-}

#include "Imports.hs"

#include "monads/M.hs"

fail :: String -> M a

#include "monads/E.hs"

type M a = E a
return = returnE
(>>=) = bindE
fail = failE

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

#include "functions/cbv/interp.hs"

#include "functions/cbv/E/lookup.hs"

#include "functions/cba/E/add.hs"

#include "functions/cbv/E/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/E.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termE
