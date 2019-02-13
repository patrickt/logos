{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}

module Data.HeightMap
  ( HeightMap (..)
  , empty
  , makeHeightMap
  ) where

import Prelude hiding (lookup)

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.Fail
import           Data.Coerce
import           Data.Proxy
import           GHC.TypeLits
import           Math.Geometry.Grid.Square
import qualified Math.Geometry.Grid as G
import qualified Math.Geometry.GridMap as GM
import qualified Math.Geometry.GridMap.Lazy as GM
import           System.Random
import Data.Maybe
import Debug.Trace

newtype HeightMap (n :: Nat) = HeightMap { unHeightMap :: GM.LGridMap RectSquareGrid Double }
  deriving (Eq, Show)

class KnownNat n => ValidSize (n :: Nat)
instance ValidSize 3
instance ValidSize 5
instance ValidSize 9
instance ValidSize 17
instance ValidSize 33
instance ValidSize 65
instance ValidSize 129
instance ValidSize 257

average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)

side :: forall n . ValidSize n => HeightMap n -> Int
side _ = fromIntegral . natVal $ Proxy @n

empty :: forall n . ValidSize n => HeightMap n
empty = let size = fromIntegral . natVal $ Proxy @n
  in HeightMap (GM.lazyGridMap (rectSquareGrid size size) (repeat 0))

insert :: (Int, Int) -> Double -> HeightMap n -> HeightMap n
insert x d (HeightMap h) = HeightMap (GM.insert x d h)

lookup :: (Int, Int) -> HeightMap n -> Maybe Double
lookup idx (HeightMap m) = GM.lookup idx m

initializeCorners :: (ValidSize n, MonadRandom m) => HeightMap n -> m (HeightMap n)
initializeCorners m = do
  let s = side m - 1
  let pts = [(0, 0), (0, s), (s, s), (s, 0)]
  let go m h = do
        rand <- getRandom
        pure $ insert h rand m
  foldM go m pts

data DisplaceParams = DisplaceParams
  { lx     :: Int
  , rx     :: Int
  , by     :: Int
  , ty     :: Int
  , spread :: Double
  }

randAroundZero :: (MonadRandom m, Random n, Num n) => n -> m n
randAroundZero spread = do
  x <- getRandom
  pure ((20 * x * spread) - spread)

jitter :: (MonadRandom m, Random n, Num n) => n -> n -> m n
jitter spread value = (+ value) <$> getRandom

makeHeightMap :: (ValidSize n, MonadFail m, MonadRandom m) => m (HeightMap n)
makeHeightMap = do
  hm <- initializeCorners empty
  let params = DisplaceParams 0 (side hm) 0 (side hm) 0.1
  displaceRecursively (side hm) params hm

displace :: forall m n . (MonadFail m, MonadRandom m) => DisplaceParams -> HeightMap n -> m (HeightMap n)
displace DisplaceParams{..} hm = do
  let cx = roundUp . average . fmap fromIntegral $ [lx, rx]
      cy = roundUp . average . fmap fromIntegral $ [by, ty]
      -- these names aren't accurate
      bottomLeft  = fromMaybe 0 $ lookup (lx, by) hm
      bottomRight = fromMaybe 0 $ lookup (rx, by) hm
      topLeft     = fromMaybe 0 $ lookup (lx, ty) hm
      topRight    = fromMaybe 0 $ lookup (rx, ty) hm


  let top = average [topLeft, topRight]
      left = average [bottomLeft, topLeft]
      bottom = average [bottomLeft, bottomRight]
      right = average [bottomRight, topRight]

  traceShowM (top, left, bottom, right)

  jbottom <- jitter bottom spread
  jtop    <- jitter top spread
  jleft   <- jitter left spread
  jright  <- jitter right spread

  let avg = average [jbottom, jtop, jleft, jright]
  let [center] = G.centre . unHeightMap $ hm
  jcenter <- jitter avg spread

  let updates = [(cx, by, jbottom), (cx, ty, jtop), (lx, cy, jleft), (rx, cy, jright)]
  let res = foldr (\(row, col, h) x -> insert (row, col) h x) hm updates

  pure $ insert center jcenter res

roundUp :: Double -> Int
roundUp x = let base = x - fromIntegral (floor x) in (if base < 0.5 then round else ceiling) x

displaceRecursively :: forall m n . (MonadFail m, MonadRandom m)
                    => Int
                    -> DisplaceParams
                    -> HeightMap n
                    -> m (HeightMap n)
displaceRecursively 3 ps h = displace ps h
displaceRecursively n ps@DisplaceParams{..} h = do
  let mid = round ((sum $ fmap fromIntegral [lx, rx]) / 2)
  pure h
    >>= displaceRecursively (n - 1) (ps { by = by - mid, lx = lx + mid})
    >>= displaceRecursively (n - 1) (ps { by = by - mid, rx = rx - mid})
    >>= displaceRecursively (n - 1) (ps { ty = ty + mid, rx = rx - mid})
    >>= displaceRecursively (n - 1) (ps { ty = ty + mid, lx = lx + mid})
  -- >>= displace (ps { by = by  mid, ry = ry - mid})
  -- >>= displace (ps { rx = rx - mid, ry = ry - mid})

-- displaceRecursively params hm = do
--   let

-- todo iterate
