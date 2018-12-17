-- Configurable ":t"

{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

module ColonT where

import Data.Typeable

class ShowType x 
 where
  showType :: x -> String

instance Typeable x => ShowType x
 where
  showType x = show $ typeOf x

instance (ShowType x, ShowType y) => ShowType (x -> y)
 where
  showType _ = 
      "(" 
    ++ showType (undefined::x)
    ++ " -> " 
    ++ showType (undefined::y) 
    ++ ")"
