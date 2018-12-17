data Value =
   Num Int
 | Fun (M Value -> M Value)
