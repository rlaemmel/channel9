{-

(C) 2010, Ralf Laemmel

We illustrate SYB style of generic programming. We exercise the
standard company examples (in some variation). See "Scrap your
boilerplate: a practical design pattern for generic programming" by
Ralf Lämmel and Simon Peyton-Jones, TLDI 2003 (and follow-up papers).

-}

module Company.SYB.Depth where

import Company.DataModel
import Data.Generics


-- Computing nesting depth of departments

depth :: Company -> Int
depth = depth'
 where
  depth' :: GenericQ Int
  depth' x = recurse x
           + mkQ 0 atDept x
   where
    recurse :: GenericQ Int
    recurse = foldr max 0 . gmapQ depth'
    atDept :: Dept -> Int
    atDept = const 1
