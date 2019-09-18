module Functor where

import Prelude hiding (Monoid, Semigroup, Functor, fmap, Applicatice)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

{-
fmap id = id
fmap (h . g) = fmap h . fmap g
-}

-- exercises
instance Functor ((->) a) where
  fmap = (.)

instance Functor [] where
  fmap = map
