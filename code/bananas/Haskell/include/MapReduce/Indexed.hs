{-# LANGUAGE ScopedTypeVariables #-}

{-

Google's MapReduce programming model revisited

(C) Ralf Laemmel, 2006--2010

-}


module MapReduce.Indexed (mapReduce, mapReduce') where

import Data.List (transpose)
import Data.Map (Map,empty,insertWith,mapWithKey,filterWithKey,toList,unionsWith)

mapReduce  ::  forall k1 k2 v1 v2. Ord k2
           =>  (k1 -> v1 -> [(k2,v2)])  -- "map" 
           ->  (k2 -> [v2] -> v2)       -- "reduce"
           ->  Map k1 v1 ->  Map k2 v2  -- I/O

mapReduce m r = reducePerKey . groupByKey . mapPerKey
 where
  mapPerKey :: Map k1 v1 -> [(k2,v2)]
  mapPerKey = concat . map (uncurry m) . toList
  groupByKey :: [(k2,v2)] -> Map k2 [v2]
  groupByKey = foldr (flip insert) empty
   where
    insert dict (k2,v2) = insertWith (++) k2 [v2] dict
  reducePerKey :: Map k2 [v2] -> Map k2 v2
  reducePerKey = mapWithKey r


-- A parallel variation

mapReduce'  ::  forall k1 k2 v1 v2. Ord k2
            =>  Int                          -- Number of reducers
            ->  (k2 -> Int)                  -- Associate keys with reducers
            ->  (k1 -> v1 -> [(k2,v2)])      -- "map" 
            ->  (k2 -> [v2] -> v2)           -- "reduce"
            ->  [Map k1 v1] ->  [Map k2 v2]  -- Distributed I/O

mapReduce' n a m r
 = map ( reducePerKey . mergeByKey )
 . transpose
 . map (
      map ( reducePerKey . groupByKey )
    . partion 
    . mapPerKey )
 where
  partion :: [(k2,v2)] -> [[(k2,v2)]]
  partion y = map (\k -> filter ((==) k . a . fst) y) [1.. n]
  mergeByKey :: [Map k2 v2] -> Map k2 [v2]
  mergeByKey = unionsWith (++) . map (mapWithKey (\_ v2 -> [v2]))
  -- **as before**
  mapPerKey :: Map k1 v1 -> [(k2,v2)]
  mapPerKey = concat . map (uncurry m) . toList
  groupByKey :: [(k2,v2)] -> Map k2 [v2]
  groupByKey = foldr (flip insert) empty
   where
    insert dict (k2,v2) = insertWith (++) k2 [v2] dict
  reducePerKey :: Map k2 [v2] -> Map k2 v2
  reducePerKey = mapWithKey r
