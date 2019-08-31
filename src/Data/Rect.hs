{-# LANGUAGE FunctionalDependencies, TemplateHaskell, TypeFamilies #-}

module Data.Rect
  ( Rect (Rect)
  , minX, midX, maxX
  , minY, midY, maxY
  , rect
  , integral
  , center
  , origin
  , translate
  , fractional
  , scaled
  , corners
  , edges
  , inset
  ) where

import Control.Lens
import Data.Point
import Data.Semilattice.Lower
import Data.Size

data Rect a = Rect
  { _origin :: !(Point a)
  , _rsize  :: !(Size a)
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
minX = origin.xPos
minY = origin.yPos

midX, midY :: Fractional a => Getter (Rect a) a
midX = to (\r -> r^.minX + (r^.width / 2))
midY = to (\r -> r^.minY + (r^.height / 2))

maxX, maxY :: Num a => Getter (Rect a) a
maxX = to (\r -> r^.minX + r^.width)
maxY = to (\r -> r^.minY + r^.height)

center :: Fractional a => Getter (Rect a) (Point a)
center = to (\r -> Point (r^.midX) (r^.midY))

integral :: (RealFrac a, Integral b) => Rect a -> Rect b
integral r = Rect (fmap round (r^.origin)) (fmap ceiling (r^.size))

fractional :: (RealFrac b, Integral a) => Rect a -> Rect b
fractional = fmap fromIntegral

translate :: Num a => Point a -> Rect a -> Rect a
translate p r = r & origin.xPos +~ p^.xPos
                  & origin.yPos +~ p^.yPos

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
