{-

(C) 2010, Ralf Laemmel

We illustrate SYB style of generic programming. We exercise the
standard company examples (in some variation). See "Scrap your
boilerplate: a practical design pattern for generic programming" by
Ralf Lämmel and Simon Peyton-Jones, TLDI 2003 (and follow-up papers).

-}

module Company.Boilerplate.Cut where

import Company.DataModel

cut :: Company -> Company
cut (n,ds) = (n,map dept ds)
 where
  dept :: Dept -> Dept
  dept (Dept n m sus) = Dept n (employee m) (map subunit sus)
  employee :: Employee -> Employee
  employee (Employee n a s) = Employee n a (s/2)  
  subunit :: SubUnit -> SubUnit
  subunit (PU e) = PU (employee e)
  subunit (DU d) = DU (dept d)
