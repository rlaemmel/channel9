main :: IO ()
main
 = do 
      let x0 = Zero
      let x1 = Succ x0
      let x2 = Succ x1
      print $ interpret x2

