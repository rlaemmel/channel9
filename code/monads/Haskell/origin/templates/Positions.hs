{-

The following code exercises Section 2.4 of the underlying article.
That section is concerned with term positions for the benefit of
improving error messages along interpretation.

-}

#include "Imports.hs"

#include "monads/M.hs"

fail :: String -> M a

#include "monads/E.hs"

#include "monads/P.hs"

type Position = Int

pos0 :: Position
pos0 = 0

type M a = P a
return = returnP
(>>=)  = bindP
fail   = failP

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"
 | At Position Term

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

#include "functions/cbv/interp.hs"
interp (At p t) e
 = resetP p (interp t e)

#include "functions/cbv/E/lookup.hs"

#include "functions/cba/E/add.hs"

#include "functions/cbv/E/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/E.hs"

#include "terms/P.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termE
      putStrLn $ test termP
