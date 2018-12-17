{-

The following code deals with the state monad (a la Section 2.5) in a
CBN monadic-style interpreter. This variation is suggested in Section
2.9.

-}

#include "Imports.hs"

#include "monads/M.hs"

tick :: M ()
fetch :: M State

#include "monads/S.hs"

type State = Int

state0 :: State
state0 = 0

type M a = S a
return = returnS
(>>=)  = bindS
tick   = tickS
fetch  = fetchS

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"
 | Count

#include "types/cbn/Value.hs"
 | Wrong

#include "types/cbn/Environment.hs"

#include "functions/cba/showval.hs"
  show Wrong = "<wrong>"

#include "functions/cbn/interp.hs"
interp Count e
 = fetch >>= \i -> return (Num i)

#include "functions/cbn/lookup.hs"

#include "functions/cba/S/add.hs"

#include "functions/cbn/S/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/S.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termS
