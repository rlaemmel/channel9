{-

We instantiate CBV monadic style with the state monad.
We use the state to maintain and query the reduction count.

-}

{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (lookup)
import Control.Monad.State

tick :: MonadState Int m => m ()
tick = get >>= put . (+1)

#include "../origin/types/cba/Name.hs"

#include "../origin/types/cba/Term.hs"
 | Count

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

interp :: MonadState Int m => Term -> Environment m -> m (Value m)
#include "functions/cbv/interp.hs"
interp Count e
 = get >>= \i -> return (Num i)

#include "functions/cbv/lookup.hs"

#include "functions/cba/S/add.hs"

#include "functions/cbv/S/apply.hs"

test :: Term -> (Value (State Int), Int)
test t = runState (interp t []) state0
 where
  state0 = 0

#include "../origin/terms/42.hs"

#include "../origin/terms/S.hs"

main 
 = do
      putStrLn $ prettyPrint (test term42)
      putStrLn $ prettyPrint (test termS)
 where
  prettyPrint (a,s1)
   = "Value: " ++ show a ++ "; " ++ 
     "Count: " ++ show s1
