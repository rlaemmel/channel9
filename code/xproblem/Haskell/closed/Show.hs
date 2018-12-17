-- Show expressions in prefix notation

module Show where

import Data

{-
instance Show Expr
 where
  show (Const i) = "Const " ++ show i
  show (Add x y) = "Add" ++ showp x ++ showp y
   where
    showp x = " (" ++ show x ++ ")"
-}
