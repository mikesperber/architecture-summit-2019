{-# LANGUAGE TypeFamilies #-}
module Image where

import Prelude hiding (Monoid, Semigroup, Functor, map, Applicative, (<*>), pure)

import Monoid
import Functor
import Applicative
import Meaning

type R = Double
  
newtype Image a = Image { imageMeaning :: Location -> a }

instance Meaning (Image a) where
  type MeaningOf (Image a) = Location -> a
  meaning = imageMeaning

type Location = (R, R)

type ImageC  = Image Color
type Region  = Image Bool

newtype Color = Color (R, R, R)

clear :: Color
clear = Color (0, 0, 0)

overColor :: Color -> Color -> Color
overColor topColor bottomColor = undefined

lift0  ::  a -> Image a
lift0 a = Image (\ location -> a)
lift1  ::  (a -> b) -> (Image a -> Image b)
lift1 f = \ image -> Image (\ location -> f ((meaning image) location))
lift2  ::  (a -> b -> c) -> (Image a -> Image b -> Image c)
lift2 f = \ image1 image2 -> Image (\ location -> f ((meaning image1) location) ((meaning image2) location))
lift3  ::  (a -> b -> c -> d) -> (Image a -> Image b -> Image c -> Image d)
lift3 f = \ image1 image2 image3  ->
            Image (\ location -> f ((meaning image1) location) ((meaning image2) location) ((meaning image3) location))

emptyImage = lift0 clear
monochrome  = lift0

over = lift2 overColor

crop region image   = cond region image emptyImage

cond   :: Image Bool -> Image a -> Image a -> Image a
cond = lift3 ifThenElse

ifThenElse condition consequent alternative = if condition then consequent else alternative

{-
instance Monoid ImageC where
  neutral  = emptyIm
  combine = over
-}

instance Monoid Color where
  neutral = undefined
  combine = undefined

instance Monoid a => Monoid (Image a) where
  neutral = lift0 neutral
  combine = lift2 combine

instance Functor Image where
  fmap = lift1

instance Applicative Image where
  pure   = lift0
  (<*>)  = lift2 ($)

{-
transform  :: Transform -> Image a -> Image a

meaning :: Transform -> ??

meaning (transform tr im)  == ??


meaning :: Transform -> ??

meaning (transform tr im) == transformS (meaning tr) (meaning im)

transformS :: ?? -> (Loc -> Color) -> (Loc -> Color)

-- exercise!
meaning :: Transform -> (Loc -> Loc)

meaning (transform tr im)  == transformS (meaning tr) (meaning im)

transformS :: (Loc -> Loc) -> (Loc -> Color) -> (Loc -> Color)

transformS h f = \ p -> f (h p)
-}

{-
mu mempty        == \ p -> mempty
mu (top <> bot)  == \ p -> mu top p <> mu bot p

mu (fmap f im)  == \ p -> f (mu im p)
                == f . mu im
mu (pure a)       == \ p -> a
mu (imf <*> imx)  == \ p -> (mu imf p) (mu imx p)

class Monad f where
  return  :: a -> f a
  join    :: f (f a) -> f a

class Functor f => Comonad f where
  coreturn  :: f a -> a
  cojoin    :: f a -> f (f a)

mu mempty        == \ p -> mempty
mu (top <> bot)  == \ p -> mu top p <> mu bot p

mu mempty        == mempty
mu (top <> bot)  == mu top <> mu bot

mu (fmap f im) == f . mu im

mu (fmap f im) == fmap f (mu im)

mu (pure a)       == \ p -> a
mu (imf <*> imx)  == \ p -> (mu imf p) (mu imx p)

mu (pure a)       == pure a
mu (imf <*> imx)  == mu imf <*> mu imx

meaning mempty    == mempty
meaning (a <> b)  == meaning a <> meaning b

a <> mempty    == a
mempty <> b    == b
a <> (b <> c)  == (a <> b) <> c

    meaning (a <> mempty)
==  meaning a <> meaning mempty
==  meaning a <> mempty
==  meaning a

    meaning (mempty <> b)
==  meaning mempty <> meaning b
==  mempty <> meaning b
==  meaning b

    meaning (a <> (b <> c))
==  meaning a <> (meaning b <> meaning c)
==  (meaning a <> meaning b) <> meaning c
==  meaning ((a <> b) <> c)
-}
