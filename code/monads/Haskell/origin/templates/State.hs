{-

The following code exercises Section 2.5 of the underlying article.
That section is concerned with the use a kind of state monad for the
purpose of a simple ticker. (Perhaps, one could think of such state as
a kind of logging/tracking facility.)

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

#include "types/cbv/Value.hs"
 | Wrong

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"
  show Wrong = "<wrong>"

#include "functions/cbv/interp.hs"
interp Count e
 = fetch >>= \i -> return (Num i)

#include "functions/cbv/lookup.hs"

#include "functions/cba/S/add.hs"

#include "functions/cbv/S/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/S.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termS
