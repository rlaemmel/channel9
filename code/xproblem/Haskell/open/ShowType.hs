module ShowType where

import ColonT
import Data
import Negation


-- Showing types concisely

instance ShowType Const
 where
  showType _ = "Expr"

instance (Expr x, Expr y) => ShowType (Add x y)
 where
  showType _ = "Expr"

instance Expr x => ShowType (Neg x)
 where
  showType _ = "Expr"

