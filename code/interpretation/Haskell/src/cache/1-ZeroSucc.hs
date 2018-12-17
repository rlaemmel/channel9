

-- There are no imports in this version.

-- Peano-like expression forms for natural numbers

data Expr
 = Zero
 | Succ Expr

-- Natural numbers as Ints
-- Riddle: Define a designated algebraic datatype!

type Nat = Int

-- Interpretation returns natural numbers.

type Value = Nat

interpret :: Expr -> Value
interpret Zero = 0
interpret (Succ x) = (interpret x) + 1

-- We don't use an algebra for interpretation in this version.

-- There is no library functionality in this version.

-- There are no auxiliary definitions that are local to this version.

-- All testing-related code, if any, resides in the main file.

main :: IO ()
main
 = do 
      let x0 = Zero
      let x1 = Succ x0
      let x2 = Succ x1
      print $ interpret x2

