{-

We advance the earlier effort regarding error messages.
That is, we add positions to terms so that error messages can refer to them.
In this manner, we need to stack together a monad.

-}

import Prelude hiding (lookup)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

type Position = Int

type M = ReaderT Position (ErrorT String Identity)

throwErrorMsg :: String -> M a
throwErrorMsg s
 = do
      p <- ask
      fail (show p ++ ": " ++ s)

#include "../origin/types/cba/Name.hs"

#include "../origin/types/cba/Term.hs"
 | At Position Term

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

interp :: Term -> Environment M -> M (Value M)
#include "functions/cbv/interp.hs"
interp (At p t) e
 = local (const p) (interp t e)

#include "functions/cbv/E/lookup.hs"

#include "functions/cba/E/add.hs"

#include "functions/cbv/E/apply.hs"

test :: Term -> Either String (Value M)
test t = runIdentity
       $ runErrorT 
       $ runReaderT (interp t []) pos0
 where
  pos0 :: Position
  pos0 = 0

#include "../origin/terms/42.hs"

#include "../origin/terms/E.hs"

#include "../origin/terms/P.hs"

main 
 = do
      putStrLn $ prettyPrint (test term42)
      putStrLn $ prettyPrint (test termE)
      putStrLn $ prettyPrint (test termP)
 where
  prettyPrint (Left s)  = "<error: " ++ s ++ ">"
  prettyPrint (Right a) = show a
