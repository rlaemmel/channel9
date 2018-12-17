{-

The following code provides execution counts for the interpreter by
means of using the state monad's type in the answer type of a
CPS-style interpreter. The underlying paper discusses this option in
Section 3.3.

-}

#include "Imports.hs"

#include "monads/M.hs"

tick :: M ()
fetch :: M State

#include "monads/N.hs"

tickN :: N ()
fetchN :: N State

#include "monads/S.hs"

type State = Int

state0 :: State
state0 = 0

type N a = S a
returnN  = returnS
bindN  = bindS
tickN  = tickS
fetchN = fetchS

#include "monads/K.hs"

type Answer = N Value

instance Show (K Value) where
  show m = show (m returnN)

type M a = K a
return   = returnK
(>>=)    = bindK
tick     = promoteK tickS
fetch    = promoteK fetchS

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
