module PrettyPrinter where

import Data


-- Operation for pretty printing

class Expr x => PrettyPrint x
 where
  prettyPrint :: x -> IO ()

instance PrettyPrint Const
 where
  prettyPrint (Const i) = putStr (show i)

instance (PrettyPrint l, PrettyPrint r) => PrettyPrint (Add l r) 
 where
  prettyPrint (Add l r) = do prettyPrint l; putStr " + "; prettyPrint r
