{-

We instantiate CBV monadic style with the list monad.
We use the list monad to add nondeterministic choice to the language.
We parameterize the main interpreter more generally.
That is, we parametrize in a MonadPlus.

-}

import Prelude hiding (lookup)
import Control.Monad

#include "../origin/types/cba/Name.hs"

#include "../origin/types/cba/Term.hs"
 | Fail
 | Amb Term Term

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

interp :: MonadPlus m => Term -> Environment m -> m (Value m)
#include "functions/cbv/interp.hs"
interp Fail e
 = mzero
interp (Amb u v) e
 = interp u e `mplus` interp v e

#include "functions/cbv/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbv/apply.hs"

test :: Term -> [Value []]
test t = interp t []

#include "../origin/terms/42.hs"

#include "../origin/terms/L.hs"

main 
 = do
      putStrLn $ show (test term42)
      putStrLn $ show (test Fail)
      putStrLn $ show (test termL)
