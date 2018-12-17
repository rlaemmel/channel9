-- Show expressions in prefix notation

module Show where

import Data

instance Show Const
 where
  show (Const i) = "Const " ++ show i

instance (Expr x, Expr y, Show x, Show y) => Show (Add x y)
 where
  show (Add x y) = "Add (" ++ show x ++ ") (" ++ show y ++ ")"
