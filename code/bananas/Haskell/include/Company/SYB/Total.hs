{-

(C) 2010, Ralf Laemmel

We illustrate SYB style of generic programming. We exercise the
standard company examples (in some variation). See "Scrap your
boilerplate: a practical design pattern for generic programming" by
Ralf Lämmel and Simon Peyton-Jones, TLDI 2003 (and follow-up papers).

-}

module Company.SYB.Total where

import Company.DataModel
import Data.Generics

total :: Company -> Float
total = everything (+) (extQ (const 0) id)
