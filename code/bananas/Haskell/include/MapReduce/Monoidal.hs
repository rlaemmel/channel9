{-# LANGUAGE ScopedTypeVariables #-}

{-

Google's MapReduce programming model revisited

(C) Ralf Laemmel, 2006--2010

The monoidal abstraction for MapReduce computations.

-}


module MapReduce.Monoidal (mapReduce, mapReduce', mapReduce'') where

import Data.Monoid

mapReduce 	:: Monoid m => (x -> m) -> [x] -> m
mapReduce f 	= foldr (mappend . f) mempty
mapReduce' 	:: Monoid m => (x -> m) -> [[x]] -> m
mapReduce' f 	= mconcat . map (mapReduce f)
mapReduce'' 	:: Monoid m => (x -> m) -> [[[x]]] -> m
mapReduce'' f 	= mconcat . map (mapReduce' f)
