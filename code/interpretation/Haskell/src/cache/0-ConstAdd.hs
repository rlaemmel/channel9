

-- There are no imports in this version.

-- Expression forms for integer constants and binary addition

data Expr
 = Const Int
 | Add Expr Expr

--  We don't use designated domains in this version.

-- We don't use a designated domain Value in this version.

evaluate :: Expr -> Int
evaluate (Const i) = i
evaluate (Add x1 x2) = evaluate x1 + evaluate x2

-- We don't use an algebra for interpretation in this version.

-- There is no library functionality in this version.

-- There are no auxiliary definitions that are local to this version.

-- All testing-related code, if any, resides in the main file.

main :: IO ()
main
 = do 
      let x1 = Const 1
      let x2 = Const 40
      let x3 = Add (Add x1 x1) x2
      print $ evaluate x3

