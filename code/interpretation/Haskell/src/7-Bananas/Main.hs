#include "../6-Monads/Main.hs"

-- Add some test cases

      let closed1 = Lambda "x" Zero
      let closed2 = Lambda "x" (Var "x")
      let open1   = Lambda "x" (Var "y")
      let open2   = Lambda "x" (Succ (Var "y"))
      print $ closedTerm closed1
      print $ closedTerm closed2
      print $ closedTerm open1
      print $ closedTerm open2

