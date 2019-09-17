module Monoid where

import Prelude hiding (Monoid, Semigroup, Functor, map, Applicative, (<*>), pure)
import Functor
import Applicative

class Monoid a where
  neutral :: a
  combine :: a -> a -> a -- closure of operations!

instance Monoid [a] where
  neutral = []
  combine list1 list2 = list1 ++ list2
  
instance Monoid b => Monoid (a -> b) where
  neutral = \ a -> neutral
  combine f g = \ a -> combine (f a) (g a)

{-
a `combine` neutral    == a
neutral `combine` b    == b
a `combine` (b `combine` c)  == (a `combine` b) `combine` c
-}
