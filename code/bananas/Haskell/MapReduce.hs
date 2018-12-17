{-

(C) 2010, Ralf Laemmel

We illustrate list homomorphisms a la Google's MapReduce.

See also this publication:

Ralf Laemmel
Google's MapReduce programming model revisited
Science of Computer Programming, 2008

-}


module MapReduce where

import qualified MapReduce.Indexed
import qualified MapReduce.Monoidal
import Data.Monoid.MapToMonoid (MapToMonoid)
import qualified Data.Monoid.MapToMonoid as MapToMonoid
import Data.Map (empty,insert,toList)
import qualified Data.Map (map)
import Data.Monoid


{- --------------------------------------------------------------------------- -}

-- Some inputs for MapReduce jobs

short_input =
       insert "doc2" "appreciate the unfold"
     $ insert "doc1" "fold the fold"
     $ empty

long_input =
     [ insert "a" "1 3 2"
     $ insert "b" "3 2 3 4 4 4 4"
     $ empty,
       insert "c" "5 7 5 5 5 6 6 6 6 6 6"
     $ insert "d" "7 5 7 7 7 7 7 8 8 8 9 8 8 8 8"
     $ insert "e" "9 8 9 9 9 9 9 9 9 10 10 10 10 11 10 10 10 10 10"
     $ empty,
       insert "f" "11 11 11 11 11 11 11 11 11 11 12"
     $ insert "g" "12 12 12 12 12 12 12 12 12 12 13 12"
     $ insert "h" "13 13 13 13 13 13 13 13 13 13 10 13 13"
     $ empty
     ]


{- --------------------------------------------------------------------------- -}

-- Testing MapReduce.Indexed

wordOccurrenceCount_Indexed = MapReduce.Indexed.mapReduce m r
 where
  m :: String -> String -> [(String,Int)]
  m = const (map (flip (,) 1) . words)
  r :: String -> [Int] -> Int
  r = const sum

main_Indexed =
       print
     $ wordOccurrenceCount_Indexed
     $ short_input


-- Testing the parallel simplistic option

wordOccurrenceCount_Indexed' = MapReduce.Indexed.mapReduce' 1 (const 1) m r
 where
  m :: String -> String -> [(String,Int)]
  m = const (map (flip (,) 1) . words)
  r :: String -> [Int] -> Int
  r = const sum

main_Indexed' =
       print
     $ wordOccurrenceCount_Indexed'
     $ long_input


{- --------------------------------------------------------------------------- -}

-- Testing MapReduce.Monoidal

doc2words
  = MapToMonoid.fromList
  . map (flip (,) (Sum 1)) 
  . words

wordOccurrenceCount_Monoidal = MapReduce.Monoidal.mapReduce doc2words

main_Monoidal =
    print
  $ wordOccurrenceCount_Monoidal
  $ map snd
  $ toList
  $ short_input


-- Testing the parallel monoidal option

wordOccurrenceCount_Monoidal' = MapReduce.Monoidal.mapReduce' doc2words

main_Monoidal' =
       print
     $ wordOccurrenceCount_Monoidal'
     $ map (map snd . toList)
     $ long_input


{- --------------------------------------------------------------------------- -}

-- Running all tests

main = do
          putStrLn "Testing MapReduce.Indexed (sequential)"
          main_Indexed
          putStrLn ""

          putStrLn "Testing MapReduce.Indexed (parallel)"
          main_Indexed'
          putStrLn ""

          putStrLn "Testing MapReduce.Monoidal (sequential)"
          main_Monoidal
          putStrLn ""

          putStrLn "Testing MapReduce.Monoidal (parallel)"
          main_Monoidal'
          putStrLn ""
