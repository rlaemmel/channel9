

import Prelude hiding (succ, pred, null)

-- We add the predecessor operation.

-- Peano-like expression forms for natural numbers

data Expr
 = Zero
 | Succ Expr

 | Pred Expr

-- Natural numbers as Ints
-- Riddle: Define a designated algebraic datatype!

type Nat = Int


-- Interpretation returns natural numbers.

type Value = Nat


interpret :: Expr -> Maybe Value
interpret Zero     = Just 0
interpret (Succ x) = (Just . (+1)) $$ interpret x
interpret (Pred x) = pred $$ interpret x
 where
  pred n | n > 0     = Just (n-1)
         | otherwise = Nothing
  
-- We don't use an algebra for interpretation in this version.

-- Partial function application

infixr 0 $$
($$) :: (a -> Maybe b) -> Maybe a -> Maybe b
($$) = maybe Nothing

-- There are no auxiliary definitions that are local to this version.

-- All testing-related code, if any, resides in the main file.

main :: IO ()
main
 = do 
      let x0 = Zero
      let x1 = Succ x0
      let x2 = Succ x1
      let x3 = Pred x0
      print $ interpret x0
      print $ interpret x1
      print $ interpret x2
      print $ interpret x3
