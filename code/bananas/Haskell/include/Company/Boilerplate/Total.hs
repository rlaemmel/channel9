{-

(C) 2010, Ralf Laemmel

We illustrate SYB style of generic programming. We exercise the
standard company examples (in some variation). See "Scrap your
boilerplate: a practical design pattern for generic programming" by
Ralf Lämmel and Simon Peyton-Jones, TLDI 2003 (and follow-up papers).

-}

module Company.Boilerplate.Total where

import Company.DataModel

total :: Company -> Float
total = sum . map dept . snd
 where
  dept :: Dept -> Float
  dept (Dept _ m sus) = sum (employee m : map subunit sus)
  employee :: Employee -> Float
  employee (Employee _ _ s) = s
  subunit :: SubUnit -> Float
  subunit (PU e) = employee e
  subunit (DU d) = dept d
