{-# LANGUAGE FunctionalDependencies, TemplateHaskell #-}

module Data.Size
  ( Size (Size)
  , HasSize (..)
  ) where

import Control.Lens.TH
import Data.Semilattice.Lower

data Size a = Size
  { _width  :: !a
  , _height :: !a
  } deriving (Eq, Functor)

instance Num a => Lower (Size a) where lowerBound = Size 0 0

instance Show a => Show (Size a) where
  showsPrec n (Size w h)
    = c '{' . showsPrec n w . c ',' . showsPrec n h . c '}' where c = showChar

makeClassy ''Size
