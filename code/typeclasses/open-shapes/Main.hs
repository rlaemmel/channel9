{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


-- Some kinds of shape as different datatypes

data Square    = Square    Int Int Int
  deriving Show
data Rectangle = Rectangle Int Int Int Int
  deriving Show
data Circle    = Circle    Int Int Float
  deriving Show
data Ellipse   = Ellipse   Int Int Float Float
  deriving Show


-- The set of all shape types

class Shape x
instance Shape Square
instance Shape Rectangle
instance Shape Circle
instance Shape Ellipse


-- The intersection operation

class (Shape x, Shape y) => Intersect x y
 where
  intersect :: x -> y -> Bool


-- Some efficient cases treated specifically

instance Intersect Square Square
 where
  intersect (Square x y l) (Square x' y' l') = undefined

instance Intersect Rectangle Rectangle
 where
  intersect (Rectangle x y h w) (Rectangle x' y' h' w') = undefined

instance Intersect Circle Circle
 where
  intersect (Circle x y r) (Circle x' y' r') = undefined

instance Intersect Ellipse Ellipse
 where
  intersect (Ellipse x y a i) (Ellipse x' y' a' i') = undefined

instance Intersect Square Rectangle
 where
  intersect (Square x y l) (Rectangle x' y' h w) = undefined


-- Intersection for all combinations of shapes in a list (nested product)

class  IntersectMany x
 where intersectMany :: x -> Bool

instance IntersectMany ()
  where  intersectMany _ = False

instance Shape x => IntersectMany (x,())
  where  intersectMany _ = False

instance ( Intersect x y
         , IntersectMany (x,z)
         , IntersectMany (y,z)
         ) => IntersectMany (x,(y,z))
 where
  intersectMany (x,(y,z))
   =  intersect x y
   || intersectMany (x,z)
   || intersectMany (y,z)


-- The subset of shapes that are normal(ized)

class Shape s => NormalShape s
instance NormalShape Square
instance NormalShape Circle


-- The normalization operation

class (Shape s1, NormalShape s2)
   => Normalize s1 s2
    | s1 -> s2
 where
  normalize :: s1 -> s2


-- Exhaustive case discrimination

{-

instance Normalize Square Square
 where
  normalize = id

instance Normalize Circle Circle
 where
  normalize = id

instance NormalShape s
      => Normalize s s
 where
  normalize = id

-}

instance ( NormalShape s1
         , Shape s2, s1 ~ s2
         )
           => Normalize s1 s2
 where
  normalize = id

instance Normalize Rectangle Square
 where
  normalize = undefined

instance Normalize Ellipse Circle
 where
  normalize = undefined


-- No testing

main = do print ()
