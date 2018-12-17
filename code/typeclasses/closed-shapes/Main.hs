-- Some kinds of shape

data Shape =
     Square    { x,y :: Int, length :: Int }
   | Rectangle { x,y :: Int, height,width :: Int }
   | Circle    { x,y :: Int, radius :: Float }
   | Ellipse   { x,y :: Int, major,minor :: Float }


-- The intersection operation

intersect :: Shape -> Shape -> Bool

-- Some efficient cases treated specifically

intersect (Square x y l)        (Square x' y' l')        = undefined
intersect (Rectangle x y h w)   (Rectangle  x' y' h' w') = undefined
intersect (Circle x y r)        (Circle x' y' r')        = undefined
intersect (Ellipse x y a i)     (Ellipse x' y' a' i')    = undefined
intersect (Square x y l)        (Rectangle x' y' h w)    = undefined

{-

Laws underlying remaining equations:
- Commutativity.
- Squares are specific rectangles.
- Circles are specific ellipses.

-}


-- A default cast

intersect s1 s2 = undefined


-- Intersection for all combinations of shapes in a list

intersectMany :: [Shape] -> Bool
intersectMany []      = False
intersectMany (x:[])  = False
intersectMany (x:y:z) =  intersect x y
                      || intersectMany (x:z)
                      || intersectMany (y:z)


-- The normalization operation

normalize :: Shape -> Shape
normalize s@(Square _ _ _)      = s
normalize   (Rectangle x y h w) = Square undefined undefined undefined
normalize c@(Circle _ _ _)      = c
normalize   (Ellipse x y a i)   = Circle undefined undefined undefined


-- No testing

main = do print ()
