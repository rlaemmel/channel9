{-# LANGUAGE FlexibleInstances #-}

{-

November 2001, Koen Claessen: This module implements some of the
combinators mentioned in the paper "Functional Programming with
Bananas, Lenses, Envelopes and Barbed Wire", by Erik Meijer, Maarten
Fokkinga, and Ross Paterson.  November 2001, Koen Claessen.

December 2010, Ralf Laemmel: I downloaded the module at some point
from Koen's website. (I don't remember the link.) I redistribute this
file (and possibly edited it) for convenience, as the C9 bananas
lecture gets to mention the abovementioned paper. At this point, the
code base for the lecture does not use this module.

-}


module Bananas where

-------------------------------------------------------------------------
-- bifunctors

class BiFunctor f where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance BiFunctor f => Functor (f a) where
  fmap f = bimap id f

-------------------------------------------------------------------------
-- product

-- I am using `(,)' for || on types, and <*> for || on values.

instance BiFunctor (,) where
  bimap = (<*>)

(<*>) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(f <*> g) (x,x') = (f x, g x')

pi1 :: (a,b) -> a
pi1 (x,y) = x

pi2 :: (a,b) -> b
pi2 (x,y) = y

(/\) :: (a -> b) -> (a -> c) -> a -> (b,c)
(f /\ g) x = (f x, g x)

-------------------------------------------------------------------------
-- sum

-- I am using `Either' for | on types, and <|> for | on values.

instance BiFunctor Either where
  bimap = (<|>)

(<|>) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
(f <|> g) (Left x)   = Left (f x)
(f <|> g) (Right x') = Right (g x')

in1 :: a -> Either a b
in1 x = Left x

in2 :: b -> Either a b
in2 y = Right y

(\/) :: (a -> c) -> (b -> c) -> Either a b -> c
(f \/ g) (Left x)  = f x
(f \/ g) (Right y) = g y

-------------------------------------------------------------------------
-- arrow

-- I am using `->' for -> on types, `==>' for -> on values,
-- and `=#=>' for -F-> on values.

(==>) :: (a -> b) -> (c -> d) -> (b -> c) -> a -> d
(f ==> g) h = g . h . f

(<==) :: (c -> d) -> (a -> b) -> (b -> c) -> a -> d
(g <== f) h = g . h . f

(=#=>) :: Functor f => (a -> f b) -> (f c -> d) -> (b -> c) -> a -> d
(f =#=> g) h = g . fmap h . f

(<=#=) :: Functor f => (f c -> d) -> (a -> f b) -> (b -> c) -> a -> d
(g <=#= f) h = g . fmap h . f

curr :: ((a,b) -> c) -> a -> b -> c
curr f x y = f (x, y)

uncurr :: (a -> b -> c) -> (a,b) -> c
uncurr f (x, y) = f x y

eval :: (a -> b, a) -> b
eval (f, x) = f x

-------------------------------------------------------------------------
-- identity

type I a = a

i :: a -> a
i f = f

type Const d a = d

-------------------------------------------------------------------------
-- varia

quest :: (a -> Bool) -> (a -> Either a a)
quest p a
  | p a       = in1 a
  | otherwise = in2 a

data Void = X

void :: a -> Void
void x = X

mu :: (a -> a) -> a
mu f = x where x = f x

-------------------------------------------------------------------------
-- recursive types

newtype Mu f = In {out :: f (Mu f)}

-- so that:
-- In  :: f (Mu f) -> Mu f
-- out :: Mu f -> f (Mu f)

-------------------------------------------------------------------------
-- recursion schemes

cata :: Functor f => (f a -> a) -> Mu f -> a
cata phi = mu (phi <=#= out)

ana :: Functor f => (a -> f a) -> a -> Mu f
ana psi = mu (In <=#= psi)

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = mu (phi <=#= psi)

para :: Functor f => (f (Mu f, a) -> a) -> Mu f -> a
para ksi = mu (\f -> ksi . fmap (i /\ f) . out)

-------------------------------------------------------------------------
-- maps

mapp :: BiFunctor f => (a -> b) -> Mu (f a) -> Mu (f b)
mapp f = cata (In . bimap f i)

-------------------------------------------------------------------------
-- free f-types

type Free f a = Mu (Dagger f a)

-- `Dagger f a x' is the type `a § x = a | f x' from the paper.

-- We need to introduce a new type here, because otherwise we
-- cannot take the fixpoint of it using `Mu'.

newtype Dagger f a b = Dagger {unDagger :: Either a (f b)}

-- We need to lift the operations on the Either type to the
-- Dagger type.

instance Functor f => BiFunctor (Dagger f) where
  bimap = (<|>>)

(<|>>) :: Functor f => (a -> b) -> (c -> d) -> Dagger f a c -> Dagger f b d
f <|>> g = Dagger . (f <|> fmap g) . unDagger

in1' :: Functor f => a -> Dagger f a b
in1' = Dagger . in1

in2' :: Functor f => f b -> Dagger f a b
in2' = Dagger . in2

(\//) :: Functor f => (a -> c) -> (f b -> c) -> Dagger f a b -> c
f \// g = (f \/ g) . unDagger

-- tau, join

tau :: Functor f => a -> Mu (Dagger f a)
tau = In . in1'

join :: Functor f => f (Mu (Dagger f a)) -> Mu (Dagger f a)
join = In . in2'

-------------------------------------------------------------------------
-- reduction

reduce :: Functor f => (f a -> a) -> Mu (Dagger f a) -> a
reduce phi = cata (i \// phi)

-------------------------------------------------------------------------
-- the end.
