{-

We instantiate CBV monadic style with the state monad.
We use the state to maintain and query the reduction count.
However, we use a special state monad to propagate backwards.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Prelude hiding (lookup)
import Control.Monad.State

newtype State' s a
      = State' { runState' :: s -> (a, s) }

instance Monad (State' s) where
  return a = State' (\s0 -> (a, s0))
  m >>= k = State' (\s2 -> let (a,s0) = runState' m s1
                               (b,s1) = runState' (k a) s2
                           in  (b,s0))

instance MonadState s (State' s) where
  get = State' (\s -> (s,s))
  put s = State' (\_ -> ((),s))

--
-- Compared to regular state, we cannot use bind in defining tick.
-- That is, the following definition loops:
-- tick = get >>= put . (+1)
-- Hence, we break encapsulation.
-- It becomes clear that a backwards state monad is a richer monad.
--
tick :: State' Int ()
tick = State' (\s -> ((),s+1))


#include "../origin/types/cba/Name.hs"

#include "../origin/types/cba/Term.hs"
 | Count

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

interp :: Term -> Environment (State' Int) -> State' Int (Value (State' Int))
#include "functions/cbv/interp.hs"
interp Count e
 = get >>= \i -> return (Num i)

#include "functions/cbv/lookup.hs"

#include "functions/cba/S/add.hs"

#include "functions/cbv/S/apply.hs"

test :: Term -> (Value (State' Int), Int)
test t = runState' (interp t []) state0
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
