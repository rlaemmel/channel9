{-

We instantiate CBV monadic style with the list monad.
In this manner, we add "call with current continuation" to the language.

-}

import Prelude hiding (lookup)
import Control.Monad.Cont

#include "../origin/types/cba/Name.hs"

#include "../origin/types/cba/Term.hs"
 | Callcc Name Term

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

interp :: MonadCont m => Term -> Environment m -> m (Value m)
#include "functions/cbv/interp.hs"
interp (Callcc x v) e
 = callCC (\k -> interp v ((x, Fun k):e))

#include "functions/cbv/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbv/apply.hs"

{-

We use "Answer = String"; we cannot use "Answer = Value" because Value
is parameterized in the monad and the monad Cont would eventually get
instantiated with Value, which triggers the occurs check. We could
only avoid this problem if we were rewriting all types and function
signatures to hardwire Cont into Value. Wonderful extensibility!

-}

test :: Term -> (Value (Cont r) -> r) -> r
test t = runCont (interp t [])

#include "../origin/terms/42.hs"

#include "../origin/terms/K.hs"

main 
 = do
      putStrLn $ test term42 show
      putStrLn $ test termK show
