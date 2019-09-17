module Map0 where

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
