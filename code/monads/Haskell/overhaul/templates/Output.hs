{-

We instantiate CBV monadic style with the writer monad.
We use the writer for an output construct in the interpreted language.

-}

{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (lookup)
import Control.Monad.Writer.Lazy

#include "../origin/types/cba/Name.hs"

#include "../origin/types/cba/Term.hs"
 | Out Term

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

interp :: MonadWriter String m => Term -> Environment m -> m (Value m)
#include "functions/cbv/interp.hs"
interp (Out u) e
 = do
      a <- interp u e
      tell (show a ++ "; ")
      return a

#include "functions/cbv/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbv/apply.hs"

test :: Term -> (Value (Writer String), String)
test t = runWriter (interp t [])

#include "../origin/terms/42.hs"

#include "../origin/terms/O.hs"

main 
 = do
      putStrLn $ show (test term42)
      putStrLn $ show (test termO)
