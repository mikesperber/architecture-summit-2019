{-# LANGUAGE TypeOperators #-}
module MapAbstract where

import Prelude hiding (Monoid, Semigroup, Functor, fmap, Applicative, (<*>), pure)

import Optional
import Functor
import Applicative
import Meaning
import TypeComposition
import TMap

type Map key = TMap key :. First

empty :: Map key value
empty = O (constant (First Absent))

insert :: Eq key => key -> value -> Map key value -> Map key value
insert key' value map = inO (update key' (First (Present value))) map

lookup :: Map key value -> key -> Optional value
lookup map key = unFirst (sample (unO map) key)

