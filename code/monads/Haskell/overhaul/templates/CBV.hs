#include "../origin/types/cba/Name.hs"

#include "../origin/types/cba/Term.hs"

#include "types/cbv/Value.hs"

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"

interp :: Monad m => Term -> Environment m -> m (Value m)
#include "functions/cbv/interp.hs"

#include "functions/cbv/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbv/apply.hs"
