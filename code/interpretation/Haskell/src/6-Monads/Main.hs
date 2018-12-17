main :: IO ()
main
 = do 
      let e = const Nothing
      let x5 = fromInt 5
      print $ run (interpret (program (Apply (Var "fac") x5))) e
