{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Map where

import Prelude hiding (Monoid, Semigroup, Functor, fmap, Applicative, (<*>), pure)

import Functor
import Applicative
import Meaning

data Optional a = Absent | Present a
  deriving (Eq, Ord, Read, Show)

newtype TMap key value = TMap { tmapMeaning :: key -> value }

instance Meaning (TMap key value) (key -> value) where
  meaning = tmapMeaning

constant :: value -> TMap key value
constant value = TMap (\ key -> value)

update :: Eq key => key -> value -> TMap key value -> TMap key value
update key' value tmap = TMap (\ key -> if key == key' then value else (tmapMeaning tmap) key)

sample :: TMap key value -> key -> value
sample tmap key = (tmapMeaning tmap) key

type Map key value = TMap key (Optional value)

empty :: Map key value
empty = constant Absent

insert :: Eq key => key -> value -> Map key value -> Map key value
insert key' value map = update key' (Present value) map

lookup :: Map key value -> key -> Optional value
lookup map key = sample map key

unionWith :: (value1 -> value2 -> value) -> TMap key value1 -> TMap key value2 -> TMap key value
unionWith function tmap1 tmap2 =
  TMap ( \ key -> function ((tmapMeaning tmap1) key) ((tmapMeaning tmap2) key))

maybeLeft :: Optional a -> Optional a -> Optional a
maybeLeft (Present a) _ = Present a
maybeLeft Absent maybe2 = maybe2

unionLeft :: Map key value -> Map key value -> Map key value
unionLeft = unionWith maybeLeft

class Monoid a where
  neutral :: a
  combine :: a -> a -> a -- closure of operations!

{-
instance Monoid value => Monoid (TMap key value) where
  neutral = constant neutral
  combine = unionWith combine
-}

instance Monoid a => Monoid (Optional a) where
  neutral = Absent
  combine a Absent = a
  combine Absent b = b
  combine (Present a) (Present b) = Present (combine a b)

newtype First a = First (Optional a)
  deriving (Eq, Ord, Read, Show)

instance Monoid a => Monoid (First a) where
  neutral = First Absent
  combine left@(First (Present _)) _ = left
  combine (First Absent) right = right 

newtype Last a = Last (Optional a)
  deriving (Eq, Ord, Read, Show)

instance Monoid a => Monoid (Last a) where
  neutral = Last Absent
  combine _ right@(Last (Present _)) = right
  combine left (Last Absent) = left

instance Monoid b => Monoid (a -> b) where
  neutral = \ a -> neutral
  combine f g = \ a -> combine (f a) (g a)

instance Monoid value => Monoid (TMap key value) where
  neutral = TMap neutral
  combine tmap1 tmap2 = TMap (combine (tmapMeaning tmap1) (tmapMeaning tmap2))

-- exercise: apply elsewhere, also binary
inTMap f tmap = TMap (f (tmapMeaning tmap))

instance Functor (TMap key) where
--  map f tmap = TMap (map f (tmapMeaning tmap))
  fmap f = inTMap (fmap f)
  
instance Applicative (TMap k) where
  pure value = TMap (\ key -> value)
  f <*> g = TMap (\ key -> ((meaning f) key) ((meaning g) key))

-- constant = pure
-- unionWith = liftApplicative2
