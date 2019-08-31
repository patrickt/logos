{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ConstraintKinds, KindSignatures, QuantifiedConstraints, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Data.HeightMap
  ( HeightMap (..)
  , ValidSize
  , empty
  , insert
  , lookup
  , mapWithKey
  , side
  , unsafeLookup
  , values
  ) where

import Prelude hiding (lookup)

import           Data.Point
import           Data.Proxy
import           GHC.Stack
import           GHC.TypeLits
import qualified Math.Geometry.Grid as G
import           Math.Geometry.Grid.Square
import qualified Math.Geometry.GridMap as GM
import qualified Math.Geometry.GridMap.Lazy as GM

newtype HeightMap = HeightMap
  { unHeightMap :: GM.LGridMap RectSquareGrid Double
  } deriving (Eq, Show)

class KnownNat n => ValidSize (n :: Nat)

-- Valid for all n where n = (2^x + 1) for some x
instance (KnownNat n, ((2 ^ Log2 (n - 1)) + 1) ~ n) => ValidSize n

-- Pass in a proof (with a proxy) that the dimensions are valid
empty :: forall n . ValidSize n => Proxy n -> HeightMap
empty n = let size = fromIntegral $ natVal n
  in HeightMap (GM.lazyGridMap (rectSquareGrid size size) (repeat 0))

insert :: Point Int -> Double -> HeightMap -> HeightMap
insert (Point x y) d (HeightMap m) = HeightMap (GM.insert (x, y) d m)

side :: HeightMap -> Int
side (HeightMap m) = fst $ G.size m

lookup :: Point Int -> HeightMap -> Maybe Double
lookup (Point x y) (HeightMap h) = GM.lookup (x, y) h

unsafeLookup :: HasCallStack => Point Int -> HeightMap -> Double
unsafeLookup p h = x where Just x = lookup p h

mapWithKey :: ((Int, Int) -> Double -> Double) -> HeightMap -> HeightMap
mapWithKey f (HeightMap hm) = HeightMap (GM.mapWithKey f hm)

values :: HeightMap -> [Double]
values (HeightMap m) = snd <$> GM.toList m

