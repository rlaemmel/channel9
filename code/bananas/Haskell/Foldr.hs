{-

(C) 2010, Ralf Laemmel (to the extent it makes sense)

We illustrate the generality and expressiveness of foldr.

The following, mostly trivial examples are inspired, to some extent,
by "A tutorial on the universality and expressiveness of fold" by
Graham Hutton, in Journal of Functional Programming, 9(4):355-372,
Cambridge University Press, July 1999, and "The Essence of the
Iterator Pattern" by Jeremy Gibbons and Bruno Oliveira, in
Mathematically-Structured Functional Programming, 2006. (In those
papers, there is much more excellent material which does not get
mentioned below or in the lecture.)

-}

module Foldr where

import Prelude hiding (words, foldr, length, sum, product, reverse, map, filter, foldl, mapM)
import qualified Prelude (words)
import Data.Traversable hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Applicative
import Data.Char

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f k [] = k
foldr f k (x:xs) = f x (foldr f k xs)

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

map :: (a -> b) -> [a] -> [b]

{-
map f [] = []
map f (x:xs) = f x : map f xs
-}

map f = foldr ((:) . f) []

length :: [a] -> Int
length = foldr ((+) . (const 1)) 0

reverse :: [a] -> [a]

{-
reverse = foldl (flip (:)) []
-}

reverse = foldr (flip (++) . (\x->[x])) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr f []
 where f x = if p x then (:) x else id

foldl :: (b -> a -> b) -> b -> [a] -> b

{-
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
-}

foldl f v xs = foldr h id xs v
 where h = (\x g -> (\a -> g (f a x)))

mapM :: Monad m => (a -> m b) -> [a] -> m [b]

{-
mapM f [] = return []
mapM f (x:xs) = f x >>= \y -> mapM f xs >>= \ys -> return (y:ys)
-}

mapM g = foldr f k
 where
  f x mys = do y <- g x; ys <- mys; return (y:ys)
  k = return []

tick :: (x -> y) -> x -> State Int y
tick f x = get >>= put . (+1) >> return (f x)

mapA :: Applicative m => (a -> m b) -> [a] -> m [b] 

{-
mapA f [] = pure [] 
mapA f (x:xs) = pure (:) <*> f x <*> traverseList f xs
-}

mapA g = foldr f k
 where
  k = pure []
  f x mys = pure (:) <*> g x <*> mys


-- Effect of function precedes the one of the argument

instance Applicative (State s)
  where
    pure = return
    mf <*> mx = mf >>= \f -> mx >>= return . f

{-

The following specification of the words function essentially uses the
fold operation to scan the input on a per-character basis, while it
maintains a pair consisting of a state (in the sense of a
deterministic, finite automaton) and the list of words encountered so
far. There are two states: False -- not currently scanning a word,
True -- the opposite.

-}

--
-- Split a string into words
-- We simply look for "spaces" as separators.
--

words :: String -> [String]
words = reverse . snd . foldl transition (False,[])
 where
  transition (state, words) char =
   if state 
     then if isSpace char
       then (not state, words)
       else (state, (head words ++ [char]) : tail words)
     else if isSpace char
       then (state, words)
       else (not state, [char]:words)


-- Test code

main =
 do
    let l1 = [1,2,3,4]
    print $ length l1
    print $ sum l1
    print $ product l1
    print $ reverse l1
    print $ map (+1) l1
    print $ map odd l1
    print $ filter odd l1
    print $ runState (mapM (tick (+1)) l1) 0
    print $ runState (mapA (tick (+1)) l1) 0
    print $ runState (traverse (tick (+1)) l1) 0
    let text = "the under-appreciated unfold" 
    print $ Prelude.words text
    print $ words text
