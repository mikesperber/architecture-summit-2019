{-# LANGUAGE TypeFamilies #-}
module Meaning where

class Meaning a where
  type MeaningOf a
  meaning :: a -> MeaningOf a
  
