{-# LANGUAGE TypeFamilies #-}
module Map0 where

-- not just yet
import Meaning

-- in a functional language, we can program with meaning

data Optional a = Absent | Present a
  deriving (Eq, Ord, Read, Show)

newtype Map key value = Map { mapMeaning :: key -> Optional value }

empty :: Map key value
empty = Map (\ key -> Absent)

insert :: Eq key => key -> value -> Map key value -> Map key value
insert key' value map =
  Map(\ key -> if key == key' then Present value else (mapMeaning map) key)

lookup :: Map key value -> key -> Optional value
lookup map key = (mapMeaning map) key

unionLeft :: Map key value -> Map key value -> Map key value
unionLeft map1 map2 =
  Map (\ key -> (mapMeaning map1) key `optionalLeft` (mapMeaning map2) key)

optionalLeft :: Optional a -> Optional a -> Optional a
optionalLeft (Present a) _ = Present a
optionalLeft Absent optional2 = optional2

-- OK, so for the next bit, we put Optional into a module

-- also: meaning

instance Meaning (Map key value) where
  type MeaningOf (Map key value) = key -> Optional value
  meaning = mapMeaning

-- but: Map combines two notions, total map and partiality
