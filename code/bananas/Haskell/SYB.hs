{-

(C) 2010, Ralf Laemmel

We illustrate SYB style of generic programming. We exercise the
standard company examples (in some variation). See "Scrap your
boilerplate: a practical design pattern for generic programming" by
Ralf Lämmel and Simon Peyton-Jones, TLDI 2003 (and follow-up papers).

-}

module SYB where

import Company.DataModel
import Company.Sample

import Company.Boilerplate.Total
import Company.Boilerplate.Cut

import qualified Company.SYB.Total as SybTotal
import qualified Company.SYB.Cut   as SybCut
import qualified Company.SYB.Depth as SybDepth

main 
 = do
-- basic tests
      print $ company == read (show company)
-- w/o SYB
      print $ total company
      print $ total (cut company)
-- w/ SYB
      print $ SybTotal.total company
      print $ SybTotal.total (SybCut.cut company)
      print $ SybDepth.depth company
