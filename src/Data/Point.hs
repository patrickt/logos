{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}

module Data.Point
  ( Point (Point)
  , HasPoint (..)
  ) where

import Control.Lens.TH
import Data.Semilattice.Lower

data Point a = Point
  { _xpos :: !a
  , _ypos :: !a
  } deriving (Eq, Functor)

instance Show a => Show (Point a) where
  showsPrec n (Point x y)
    = c '{' . showsPrec n x . c ',' . showsPrec n y . c '}' where c = showChar

instance Num a => Lower (Point a) where lowerBound = Point 0 0

makeClassy ''Point
