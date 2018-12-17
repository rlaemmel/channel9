{-

We instantiate CBV monadic style with the identity monad.

-}

import Prelude hiding (lookup)
import Control.Monad.Identity

#include "cache/CBV.hs"

test :: Term -> Value Identity
test t = runIdentity (interp t [])

#include "../origin/terms/42.hs"

#include "../origin/terms/E.hs"

main 
 = do
      putStrLn $ show (test term42)
      putStrLn $ show (test termE)

