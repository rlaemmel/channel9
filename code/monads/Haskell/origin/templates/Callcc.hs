{-

The following code exercises Section 3.2 of the underlying article.
That section is concerned with call with current continuation.

-}

#include "Imports.hs"

#include "monads/M.hs"

callCC :: ((a -> M b) -> M a) -> M a

#include "monads/K.hs"

type Answer = Value

instance Show (M Value) where
  show m = show (m id)

type M a = K a
return   = returnK
(>>=)    = bindK
callCC   = callCCK

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"
 | Callcc Name Term

#include "types/cbv/Value.hs"
 | Wrong

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"
  show Wrong = "<wrong>"

#include "functions/cbv/interp.hs"
interp (Callcc x v) e
 = callCC (\k -> interp v ((x, Fun k):e))

#include "functions/cbv/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbv/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/K.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termK
