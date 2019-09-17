module Intro where

data Liveness = Dead | Alive
  deriving Show

data Animal weight =
    Dillo { dilloLiveness :: Liveness,
            dilloWeight   :: weight }
  | Parrot String weight
  deriving Show

data Weight = Kg Integer | Pound Integer
  deriving Show

instance Num Weight where
  (Kg a) + (Kg b) = Kg (a + b)

mapAnimal :: (a -> b) -> Animal a -> Animal b
mapAnimal f (Dillo liveness weight) = Dillo liveness (f weight)
mapAnimal f (Parrot sentence weight) = Parrot sentence (f weight)

instance Functor Animal where
    fmap = mapAnimal

-- Tier überfahren
runOverAnimal :: Animal w -> Animal w
runOverAnimal (Dillo _ w) = Dillo Dead w
runOverAnimal (Parrot _ w) = Parrot ""  w

d1' = Parrot "Hallo!" (Kg 5)
feedAnimal amount = fmap (amount +)
