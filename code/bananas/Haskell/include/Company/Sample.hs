{-

(C) 2010, Ralf Laemmel

We illustrate SYB style of generic programming. We exercise the
standard company examples (in some variation). See "Scrap your
boilerplate: a practical design pattern for generic programming" by
Ralf Lämmel and Simon Peyton-Jones, TLDI 2003 (and follow-up papers).

-}

module Company.Sample where

import Company.DataModel

company = 
 ( "meganalysis"
 , [ Dept "Research" 
      (Employee "Craig" "Redmond" 123456)
      [ PU (Employee "Erik" "Utrecht" 12345)
      , PU (Employee "Ralf" "Koblenz" 1234)
      ]
   , Dept "Development"
      (Employee "Ray" "Redmond" 234567)
       [ DU (Dept "Dev1"
             (Employee "Klaus" "Boston" 23456)
             [ DU (Dept "Dev1.1"
                   (Employee "Karl" "Riga" 2345)
                   [ PU (Employee "Joe" "Wifi City" 2344)
                   ])
             ])
       ]
   ]
 )
