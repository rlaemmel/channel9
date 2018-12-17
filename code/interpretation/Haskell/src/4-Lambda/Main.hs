main :: IO ()
main
 = do 
      let e = const Nothing
      let x5 = fromInt 5
      print $ interpret (Apply fac x5) e

