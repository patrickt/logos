{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ConstraintKinds, KindSignatures, QuantifiedConstraints, ScopedTypeVariables, TemplateHaskell,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.HeightMap
  ( HeightMap (..)
  , ValidSize
  , empty
  , lookup
  , unsafeLookup
  , insert
  , makeHeightMap
  ) where

import Prelude hiding (lookup)

import           Control.Effect
import           Control.Effect.Random
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Lens hiding (view, views)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Point
import           Data.Proxy
import           Data.Rect as Rect
import           Data.Size
import GHC.Stack
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
instance (KnownNat n, (2 ^ (Log2 (n - 1))) ~ (n - 1)) => ValidSize n

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

values :: HeightMap -> [Double]
values (HeightMap m) = snd <$> GM.toList m

type Spread = Double

type Displacement sig m =
  ( Member (State HeightMap) sig
  , Member (Reader (Rect Int)) sig
  , Member (Reader Spread) sig
  , Carrier sig m, MonadRandom m
  )

initializeCorners :: Displacement sig m => m ()
initializeCorners = do
  bounds <- asks Rect.corners
  forM_ bounds $ \point -> do
    rand <- getRandom
    rand <$ modify (insert point rand)

jitter :: Displacement sig m => Double -> m Double
jitter val = do
  munge <- ask @Spread
  rand  <- getRandom
  let aroundZero = (munge * rand * 2) - munge
  pure (val + aroundZero)

setEdges :: Displacement sig m => m [Double]
setEdges = do
  vals <- asks Rect.corners >>= traverse (gets . unsafeLookup)
  let [tl, tr, bl, br] = vals

  let top    = avg tl tr
      right  = avg tr br
      bottom = avg bl br
      left   = avg tl bl
      avg a b = (a + b) / 2.0

  bounds <- fmap (fromIntegral @_ @Double) <$> ask @(Rect Int)
  let vals = zip [top, right, bottom, left] (Rect.edges bounds)
  forM vals $ \(avg, pos) -> do
    jittered <- jitter avg
    jittered <$ modify (insert (fmap ceiling pos) jittered)

setCenter :: Displacement sig m => [Double] -> m ()
setCenter vals = do
  let avg = sum vals / fromIntegral (length vals)
  jittered <- jitter avg
  rect <- ask @(Rect Int)
  let cent = (fmap (fromIntegral @_ @Double) rect) ^. Rect.center
  modify (insert (fmap round cent) jittered)

-- multiply :: Displacement sig m => m ()
-- multiply = modify (\(HeightMap hm) -> HeightMap (GM.mapWithKey (\_ x -> x * 10) hm))

normalize :: Displacement sig m => m ()
normalize = do
  vals <- gets values
  let min, max, span :: Double
      min = minimum vals
      max = maximum vals
      span = max - min
      compensate _ level = (10 * (level - min) / span)
  modify (\(HeightMap hm) -> HeightMap (GM.mapWithKey compensate hm))

makeHeightMap :: ValidSize n => Proxy n -> IO HeightMap
makeHeightMap p = let hm = empty p in
  liftIO
  . runM
  . evalRandomIO
  . execState hm
  . runReader (0.3 :: Spread)
  . runReader (Rect.rect 0 0 (side hm - 1) (side hm - 1))
  $ churn

-- If I was smarter I would do this as the hylomorphism it very clearly is
churn :: Displacement sig m => m ()
churn = initializeCorners *> go *> normalize where
  go = do
    let recur f x = local @(Rect Int) f (go *> x)
    let half x = x `div` 2
    rect <- ask @(Rect Int)
    when (rect^.width >= 2) $ do
      newside <- asks @(Rect Int) (^.width.to half)
      setEdges >>= setCenter
      local @Spread (/ 2.0)
        . recur (size.mapped %~ half)
        . recur (origin.x +~ newside)
        . recur (origin.y +~ newside)
        . recur (origin.x -~ newside)
        $ pure ()
