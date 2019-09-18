module Optional where

import Prelude hiding (Monoid, Semigroup, Functor, map, Applicative, (<*>), pure)

import Monoid

data Optional a = Absent | Present a
  deriving (Eq, Ord, Read, Show)

-- exercise: functor

optionalLeft :: Optional a -> Optional a -> Optional a
optionalLeft (Present a) _ = Present a
optionalLeft Absent optional2 = optional2

instance Monoid a => Monoid (Optional a) where
  neutral = Absent
  combine a Absent = a
  combine Absent b = b
  combine (Present a) (Present b) = Present (combine a b)

newtype First a = First (Optional a)
  deriving (Eq, Ord, Read, Show)

unFirst (First optional) = optional

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
