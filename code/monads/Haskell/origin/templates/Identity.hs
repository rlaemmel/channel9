{-

The following code exercises Section 2.2 of the underlying article.
That section is concerned with the use of the identity monad for the
instantiation of the monadic-style interpreter. We do *not* import
Control.Monad.Identity. Instead, we follow the original paper closely
For the identity monad, see here:

http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/src/Control-Monad-Identity.html

-}

#include "Imports.hs"

#include "monads/M.hs"

#include "monads/I.hs"

type M a = I a
return = returnI
(>>=) = bindI

#include "cache/CBV.hs"

#include "terms/42.hs"

#include "terms/E.hs"

main 
 = do
      putStrLn $ test term42
      putStrLn $ test termE
