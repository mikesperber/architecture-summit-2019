{-# LANGUAGE TypeFamilies #-}
module Map where

-- not just yet
import Prelude hiding (Monoid, Semigroup, Functor, fmap, Applicative, (<*>), pure)

import Optional
import Meaning

-- not just yet
import Monoid
import Functor
import Applicative

newtype TMap key value = TMap { tmapMeaning :: key -> value }

instance Meaning (TMap key value) where
  type MeaningOf (TMap key value) = key -> value
  meaning = tmapMeaning

constant :: value -> TMap key value
constant value = TMap (\ key -> value)

update :: Eq key => key -> value -> TMap key value -> TMap key value
update key' value tmap = TMap (\ key -> if key == key' then value else (meaning tmap) key)

sample :: TMap key value -> key -> value
sample tmap key = (meaning tmap) key

type Map key value = TMap key (Optional value)

empty :: Map key value
empty = constant Absent

insert :: Eq key => key -> value -> Map key value -> Map key value
insert key' value map = update key' (Present value) map

lookup :: Map key value -> key -> Optional value
lookup map key = sample map key

unionLeft' :: Map key value -> Map key value -> Map key value
unionLeft' tmap1 tmap2 = TMap ( \ key -> ((meaning tmap1) key) `optionalLeft` ((meaning tmap2) key))

unionWith :: (value1 -> value2 -> value) -> TMap key value1 -> TMap key value2 -> TMap key value
unionWith combine tmap1 tmap2 =
  TMap ( \ key -> ((meaning tmap1) key) `combine` ((meaning tmap2) key))

unionLeft :: Map key value -> Map key value -> Map key value
unionLeft = unionWith optionalLeft

-- now import Monoid

-- this works:
{-
instance Monoid value => Monoid (TMap key value) where
  neutral = constant neutral
  combine = unionWith combine
-}

-- add Monoid instance for function

instance Monoid value => Monoid (TMap key value) where
  neutral = TMap neutral
  combine tmap1 tmap2 = TMap ((meaning tmap1) `combine` (meaning tmap2))

-- exercise: do we see this elsewhere
inTMap f tmap = TMap (f (meaning tmap))

-- at this point, we do Functor
instance Functor (TMap key) where
--  map f tmap = TMap (map f (meaning tmap))
  fmap f = inTMap (fmap f)

-- at this point, we do applicative
instance Applicative (TMap k) where
  pure value = TMap (\ key -> value)
  f <*> a = TMap (\ key -> ((meaning f) key) ((meaning a) key))

-- constant = pure
-- unionWith = liftApplicative2

-- problem with the laws:

{-
instance_sem Functor (Map k) where
  [[fmap f m]] = fmap (fmap f) [[m]]

not a morphism!
-}
