{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module TypeComposition where

import Prelude hiding (Monoid, Semigroup, Functor, map, Applicative, (<*>), pure)

import Functor
import Applicative

newtype ((g :: k2 -> *) :. (f :: k1 -> k2)) (a :: k1)
  = O (g (f a))
  deriving (Eq, Show, Ord)

unO :: (g :. f) a -> g (f a)
unO (O gfa) = gfa

-- | Apply a unary function within the 'O' constructor.
inO :: (g (f a) -> g' (f' a')) -> ((g :. f) a -> (g' :. f') a')
inO f = \ (O x) -> O (f x)

-- | Apply a binary function within the 'O' constructor.
inO2 :: (g (f a)   -> g' (f' a')   -> g'' (f'' a''))
     -> ((g :. f) a -> (g' :. f') a' -> (g'' :. f'') a'')
inO2 f = \ (O x) (O x') -> O (f x x')

-- | Apply a ternary function within the 'O' constructor.
inO3 :: (g (f a)   -> g' (f' a')   -> g'' (f'' a'')   -> g''' (f''' a'''))
     -> ((g :. f) a -> (g' :. f') a' -> (g'' :. f'') a'' -> (g''' :. f''') a''')
inO3 f = \ (O x) (O x') (O x'') -> O (f x x' x'')

fmapFF :: (  Functor g,   Functor f) => (a -> b) -> (g :. f) a -> (g :. f) b
fmapFF = inO . map . map

instance (Functor g, Functor f) => Functor (g :. f) where
  -- map = fmapFF
  map f (O gfa) = O (map (map f) gfa)

instance (Applicative g, Applicative f) => Applicative (g :. f) where
  pure  = O . pure . pure
  (<*>) = (inO2 . liftApplicative2) (<*>)
