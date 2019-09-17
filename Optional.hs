module Optional where

import Prelude hiding (Monoid, Semigroup, Functor, map, Applicative, (<*>), pure)

import Monoid

data Optional a = Absent | Present a
  deriving (Eq, Ord, Read, Show)

maybeLeft :: Optional a -> Optional a -> Optional a
maybeLeft (Present a) _ = Present a
maybeLeft Absent maybe2 = maybe2

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
