{-# LANGUAGE DeriveDataTypeable #-}

{-

(C) 2010, Ralf Laemmel

We illustrate SYB style of generic programming. We exercise the
standard company examples (in some variation). See "Scrap your
boilerplate: a practical design pattern for generic programming" by
Ralf Lämmel and Simon Peyton-Jones, TLDI 2003 (and follow-up papers).

-}

module Company.DataModel where

import Data.Typeable
import Data.Data

type Company = (Name, [Dept])
data Dept = Dept Name Manager [SubUnit]
 deriving (Eq, Read, Show, Typeable, Data)
type Manager = Employee
data Employee = Employee Name Address Salary
 deriving (Eq, Read, Show, Typeable, Data)
data SubUnit = PU Employee | DU Dept
 deriving (Eq, Read, Show, Typeable, Data)
type Name = String
type Address = String
type Salary = Float
