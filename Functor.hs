module Functor where

import Prelude hiding (Monoid, Semigroup, Functor, map, Applicatice)

class Functor f where
  map :: (a -> b) -> f a -> f b

{-
map id = id
map (h . g) = map h . map g
-}

instance Functor ((->) a) where
  map = (.)
