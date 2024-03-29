module Applicative where
  
import Prelude hiding (Monoid, Semigroup, Functor, map, Applicative, (<*>), pure)

import Functor

infixl 4 <*>

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

{-
pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
-}

-- exercise
liftApplicative2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
liftApplicative2 f a b = (pure f) <*> a <*> b

liftApplicative3 :: Applicative f => (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
liftApplicative3 f a b c = (pure f) <*> a <*> b <*> c

-- exercise
instance Applicative ((->) a) where
  pure a     = \ _ -> a
  fs <*> xs  = \ u -> (fs u) (xs u)

