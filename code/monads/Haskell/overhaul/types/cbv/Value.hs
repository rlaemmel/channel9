data Value m =
   Wrong
 | Num Int
 | Fun (Value m -> m (Value m))
