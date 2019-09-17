{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Image0 where

import Meaning

type R = Double
  
newtype Image = Image { imageMeaning :: Location -> Color }

instance Meaning Image where
  type MeaningOf Image = Location -> Color
  meaning = imageMeaning

data Bitmap = Bitmap

fromBitmap  :: Bitmap -> Image
fromBitmap bitmap = undefined
toBitmap    :: Image -> Bitmap
toBitmap image = undefined

type Color = (R, R, R)

clear :: Color
clear = (0, 0, 0)

emptyImage :: Image
emptyImage = Image (\ location -> clear)

monochrome :: Color -> Image
monochrome color = Image (\ location -> color)

circle      :: R -> Color -> Image
circle = \ radius color -> Image (\location -> if magnitude location <= radius then color else clear)

square      :: R -> Color -> Image
square = \ size color -> Image (\ (x, y) -> let half = size / 2
                                            in if x >= -half && x <= half && y >= -half && y <= half
                                               then color
                                               else clear)
-- more shapes etc.
over :: Image -> Image -> Image
over imageTop imageBottom = Image (\ location -> overColor ((meaning imageTop) location) ((meaning imageBottom) location))

newtype Region = Region { regionMeaning :: Location -> Bool }

crop :: Region -> Image -> Image
crop region image = Image (\ location -> if (regionMeaning region) location
                                         then (meaning image) location
                                              else clear)

type Location = (R, R)

magnitude :: Location -> R
magnitude (x, y) = sqrt (x*x + y*y)


overColor :: Color -> Color -> Color
overColor topColor bottomColor = undefined

-- meaning (over top bot)  == overS (meaning top) (meaning bot)
-- meaning (crop reg im)   == cropS (meaning reg) (meaning im)

overS :: (Location -> Color) -> (Location -> Color) -> (Location -> Color)
overS f g = \ p -> overColor (f p) (g p)
cropS :: (Location -> Bool) -> (Location -> Color) -> (Location -> Color)
cropS f g = \ p -> if f p then g p else clear

inImage f = \ image -> Image (f (meaning image))
  
inImage2 :: ((Location -> Color) -> (Location -> Color) -> (Location -> Color))
              -> (Image -> Image -> Image)
inImage2 f = \ image1 image2 -> Image (f (meaning image1) (meaning image2))

-- then reformulate over, crop in terms of this ... FIXME: only over works here

-- FIXME: meaning typeclass, formulate equations in terms of them
