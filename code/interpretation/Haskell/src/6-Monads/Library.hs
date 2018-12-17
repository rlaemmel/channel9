#include "../5-Letrec/Library.hs"


-- Get a Maybe out of a computation

run :: Computation t -> Env -> Maybe t
run t e = runIdentity (runMaybeT (runReaderT t e))


-- Turn a plain Maybe into a computation

try :: Maybe a -> Computation a
try = maybe (fail undefined) return

