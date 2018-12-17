{-

The following code provides error messages for the interpreter by
means of using the error monad's type in the answer type of a
CPS-style interpreter. The underlying paper discusses this option in
Section 3.3.

-}

#include "Imports.hs"

#include "monads/M.hs"

fail :: String -> M a

#include "monads/N.hs"

failN :: String -> N a

#include "monads/E.hs"

type N a = E a
returnN  = returnE
bindN    = bindE
failN    = failE

#include "monads/K.hs"

type Answer = N Value

instance Show (K Value) where
  show m = show (m returnN)

type M a = K a
return   = returnK
(>>=)    = bindK
fail s   = promoteK (failE s)

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

#include "functions/cbv/interp.hs"

#include "functions/cbv/E/lookup.hs"

#include "functions/cba/E/add.hs"

#include "functions/cbv/E/apply.hs"

#include "functions/cba/test.hs"

#include "terms/42.hs"

#include "terms/E.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termE
