{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Meaning where

class Meaning a b | a -> b where
  meaning :: a -> b
  
