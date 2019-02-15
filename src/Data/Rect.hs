{-# LANGUAGE TemplateHaskell, TypeFamilies, FunctionalDependencies #-}

module Data.Rect
  ( Point (Point)
  , xpos
  , ypos
  , zero
  , Size (Size)
  , HasSize (..)
  , Rect (Rect)
  , midX
  , midY
  , rect
  , integral
  , origin
  , translate
  , scaled
  , corners
  , edges
  , inset
  ) where

import Prelude
import Control.Lens
import Data.Semilattice.Lower

zero :: Lower a => a
zero = lowerBound

data Point a = Point
  { _xpos :: !a
  , _ypos :: !a
  } deriving (Eq, Functor)

instance Show a => Show (Point a) where
  showsPrec n (Point x y)
    = c '{' . showsPrec n x . c ',' . showsPrec n y . c '}' where c = showChar

instance Num a => Lower (Point a) where lowerBound = Point 0 0

makeClassy ''Point

data Size a = Size
  { _width  :: !a
  , _height :: !a
  } deriving (Eq, Functor)

instance Num a => Lower (Size a) where lowerBound = Size 0 0

instance Show a => Show (Size a) where
  showsPrec n (Size w h)
    = c '{' . showsPrec n w . c ',' . showsPrec n h . c '}' where c = showChar

makeClassy ''Size

data Rect a = Rect
  { _origin   :: !(Point a)
  , _rsize :: !(Size a)
  } deriving (Eq, Functor)

instance Show a => Show (Rect a) where
  showsPrec n (Rect o s)
    = c '{' . showsPrec n o . c ',' . showsPrec n s . c '}' where c = showChar

instance Num a => Lower (Rect a) where lowerBound = Rect lowerBound lowerBound

makeLenses ''Rect

instance HasSize (Rect a) a where size = rsize

rect :: a -> a -> a -> a -> Rect a
rect a b c d = Rect (Point a b) (Size c d)

minX, minY :: Getter (Rect a) a
minX = origin.xpos
minY = origin.ypos

midX, midY :: Fractional a => Getter (Rect a) a
midX = to (\r -> r^.minX + (r^.width / 2))
midY = to (\r -> r^.minY + (r^.height / 2))

maxX, maxY :: Num a => Getter (Rect a) a
maxX = to (\r -> r^.minX + r^.width)
maxY = to (\r -> r^.minY + r^.height)

integral :: (RealFrac a, Integral b) => Rect a -> Rect b
integral r = Rect (fmap round (r^.origin)) (fmap ceiling (r^.size))

translate :: Num a => Point a -> Rect a -> Rect a
translate p r = r & origin.xpos +~ p^.xpos
                  & origin.ypos +~ p^.ypos

inset :: Num a => a -> Rect a -> Rect a
inset p r = r & size.width -~ p
              & size.height -~ p

scaled :: Num a => a -> Rect a -> Rect a
scaled f r = r & size.mapped *~ f

edges :: Fractional a => Rect a -> [Point a]
edges r = [top, right, bottom, left] where
  top = Point (r^.midX) (r^.minY)
  right = Point (r^.maxX) (r^.midY)
  bottom = Point (r^.midX) (r^.maxY)
  left = Point (r^.minX) (r^.midY)

corners :: Num a => Rect a -> [Point a]
corners r = [topleft, topright, bottomright, bottomleft] where
  topleft = r^.origin
  topright = Point (r^.maxX) (r^.minY)
  bottomright = Point (r^.maxX) (r^.maxY)
  bottomleft = Point (r^.minX) (r^.maxY)

-- icorners :: Num a => Rect a -> [Point a]
-- icorners r = [topleft, topright, bottomright, bottomleft] where
--   topleft = r^.origin
--   topright = Point (r^.maxX - 1) (r^.minY)
--   bottomright = Point (r^.maxX - 1) (r^.maxY - 1)
--   bottomleft = Point (r^.minX) (r^.maxY - 1)
