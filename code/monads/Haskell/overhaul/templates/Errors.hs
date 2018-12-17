{-

We instantiate CBV monadic style with the error monad.
We use the existing library implementation based on Either.
We also revise some meanings to produce useful error messages.

-}

import Prelude hiding (lookup)
import Control.Monad.Error

throwErrorMsg :: (Error e, MonadError e m) => String -> m a
throwErrorMsg = fail . strMsg

#include "../origin/types/cba/Name.hs"

#include "../origin/types/cba/Term.hs"

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

interp :: (Error e, MonadError e m) => Term -> Environment m -> m (Value m)
#include "functions/cbv/interp.hs"

#include "functions/cbv/E/lookup.hs"

#include "functions/cba/E/add.hs"

#include "functions/cbv/E/apply.hs"

test :: Term -> Either String (Value (Either String))
test t = interp t []

#include "../origin/terms/42.hs"

#include "../origin/terms/E.hs"

main 
 = do
      putStrLn $ prettyPrint (test term42)
      putStrLn $ prettyPrint (test termE)
 where
  prettyPrint (Left s)  = "<error: " ++ s ++ ">"
  prettyPrint (Right a) = show a
