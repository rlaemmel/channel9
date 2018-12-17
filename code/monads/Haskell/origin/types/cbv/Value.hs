data Value =
   Num Int
 | Fun (Value -> M Value)
