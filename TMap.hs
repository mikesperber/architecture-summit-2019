{-# LANGUAGE TypeFamilies #-}
module TMap where

import Prelude hiding (Monoid, Semigroup, Functor, fmap, Applicative, (<*>), pure)

import Functor
import Applicative
import Monoid
import Meaning

newtype TMap key value = TMap { tmapMeaning :: key -> value }

instance Meaning (TMap key value) where
  type MeaningOf (TMap key value) = key -> value
  meaning = tmapMeaning

update :: Eq key => key -> value -> TMap key value -> TMap key value
update key' value tmap = TMap (\ key -> if key == key' then value else (tmapMeaning tmap) key)

sample :: TMap key value -> key -> value
sample tmap key = (tmapMeaning tmap) key

instance Monoid value => Monoid (TMap key value) where
  neutral = TMap neutral
  combine tmap1 tmap2 = TMap (combine (tmapMeaning tmap1) (tmapMeaning tmap2))

inTMap f tmap = TMap (f (tmapMeaning tmap))

instance Functor (TMap key) where
--  fmap f tmap = TMap (map f (tmapMeaning tmap))
  fmap f = inTMap (fmap f)
  
instance Applicative (TMap k) where
  pure value = TMap (\ key -> value)
  f <*> g = TMap (\ key -> ((meaning f) key) ((meaning g) key))

constant :: value -> TMap key value
constant = pure
unionWith :: (value1 -> value2 -> value) -> TMap key value1 -> TMap key value2 -> TMap key value
unionWith = liftApplicative2
