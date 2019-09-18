{-# LANGUAGE TypeFamilies #-}
module Image where

import Prelude hiding (Monoid, Semigroup, Functor, map, Applicative, (<*>), pure)

import Monoid
import Functor
import Applicative
import Meaning

type R = Double
type Location = (R, R)
  
newtype Image a = Image { imageMeaning :: Location -> a }

instance Meaning (Image a) where
  type MeaningOf (Image a) = Location -> a
  meaning = imageMeaning

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

