{-

The following code exercises Section 2.8 of the underlying article.
That section is concerned with propagating state backwards.

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
(>>=)  = bindS'
tick   = tickS
fetch  = fetchS

bindS' :: S a -> (a -> S b) -> S b
m `bindS'` k = \s2 -> let (a,s0) = m s1
                          (b,s1) = k a s2
                      in  (b,s0)

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
