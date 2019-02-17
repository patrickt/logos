{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, FunctionalDependencies #-}

module Data.Point
  ( Point (Point)
  , HasPoint (..)
  , forGrid
  , forGrid'
  , fromGrid
  , distanceBetween
  ) where

import Control.Lens.TH
import Control.Lens.Getter
import Data.Semilattice.Lower

data Point a = Point
  { _x :: !a
  , _y :: !a
  } deriving (Eq, Functor)

instance Show a => Show (Point a) where
  showsPrec n (Point x y)
    = c '{' . showsPrec n x . c ',' . showsPrec n y . c '}' where c = showChar

instance Num a => Lower (Point a) where lowerBound = Point 0 0

makeClassy ''Point

forGrid, forGrid' :: Point Int -> (Int, Int)
forGrid  (Point a b) = (a, b)
forGrid' = fmap succ . forGrid

fromGrid :: (Int, Int) -> Point Int
fromGrid (col, row) = Point col row

distanceBetween :: forall a. Integral a => Point a -> Point a -> Double
distanceBetween a b = sqrt (square (b^.x - a^.x) + square (b^.y - a^.y))
  where square n = (fromIntegral @a @Double n) ^^ 2
