main :: IO ()
main
 = do 
      let x1 = Const 1
      let x2 = Const 40
      let x3 = Add (Add x1 x1) x2
      print $ evaluate x3

