{-

This is Fig. 1 of the underlying paper.

-}

#include "types/cba/Name.hs"

#include "types/cba/Term.hs"

#include "types/cbv/Value.hs"
 | Wrong

#include "types/cbv/Environment.hs"

#include "functions/cba/showval.hs"
  show Wrong = "<wrong>"

#include "functions/cbv/interp.hs"

#include "functions/cbv/lookup.hs"

#include "functions/cba/add.hs"

#include "functions/cbv/apply.hs"

#include "functions/cba/test.hs"
