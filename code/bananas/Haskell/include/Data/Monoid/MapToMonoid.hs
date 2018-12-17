{-

(C) Ralf Laemmel, 2010

This module provides a monoid for maps (dictionaries). Data.Map
readily defines a monoid instance for maps, but it does not assume
that the value domain is a monoid by itself. In the context of
map-reduce style of programming we need this additional constraint,
and hence, a different monoid instance.

-}


module Data.Monoid.MapToMonoid (
 MapToMonoid,
 fromList,
 toList
) where

import Data.Monoid
import Data.Map (Map)
import qualified Data.Map (fromListWith, toList, unionWith)


{- --------------------------------------------------------------------------- -}

newtype (Ord k, Monoid v) =>
        MapToMonoid k v =
        MapToMonoid { getMap :: Map k v }

toList :: (Ord k, Monoid v) => MapToMonoid k v -> [(k,v)]
toList = Data.Map.toList . getMap

fromList :: (Ord k, Monoid v) => [(k,v)] -> MapToMonoid k v
fromList = MapToMonoid . Data.Map.fromListWith mappend

instance (Ord k, Monoid v) => Monoid (MapToMonoid k v)
 where
  mempty = MapToMonoid mempty
  mappend (MapToMonoid f)
          (MapToMonoid g)
         = MapToMonoid (Data.Map.unionWith mappend f g)

instance (Ord k, Monoid v, Show k, Show v) => Show (MapToMonoid k v)
 where
  show = show . getMap

{- --------------------------------------------------------------------------- -}
