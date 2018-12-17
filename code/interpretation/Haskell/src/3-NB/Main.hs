main :: IO ()
main
 = do 
      let x0 = Zero
      let x1 = Succ x0
      let x2 = IsZero x1
      let x3 = Cond x2 x0 x1
      print $ interpret x3
