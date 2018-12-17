#include "../3-NB/Algebra.hs"

var :: Env -> String -> Maybe Value
var = ($)

lambda :: Env -> String -> (Env -> Maybe Value) -> Maybe Value
lambda e n f = Just (InFun (\r -> f (modify e n (Just r))))

apply :: Maybe Value -> Maybe Value -> Maybe Value
apply f a 
 = (\f' -> 
   (\a' -> (flip ($) a') 
     $$ outFun f') 
     $$ a) 
     $$ f

