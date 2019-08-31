{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module Logos.Terrain
  ( generateTerrain
  ) where

import Control.Effect
import Control.Effect.Random
import Control.Effect.Reader
import Control.Effect.State
import Control.Lens hiding (views)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.HeightMap
import Data.Proxy
import Data.Point
import Data.Rect (Rect)
import qualified Data.Rect as Rect
import Data.Size
import Data.Traversable
import Instances ()

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
  for_ bounds $ \pt -> do
    rand <- getRandom
    rand <$ modify (insert pt rand)

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
  let pairs = zip [top, right, bottom, left] (Rect.edges bounds)
  for pairs $ \(average, pos) -> do
    jittered <- jitter average
    jittered <$ modify (insert (fmap ceiling pos) jittered)

setCenter :: Displacement sig m => [Double] -> m ()
setCenter vals = do
  let avg = sum vals / fromIntegral (length vals)
  jittered <- jitter avg
  cent <- asks @(Rect Int) (view Rect.center . Rect.fractional @Double)
  modify (insert (fmap round cent) jittered)

-- multiply :: Displacement sig m => m ()
-- multiply = modify (\(HeightMap hm) -> HeightMap (GM.mapWithKey (\_ x -> x * 10) hm))

normalize :: Displacement sig m => m ()
normalize = do
  vals <- gets values
  let minh, maxh, spanh :: Double
      minh = minimum vals
      maxh = maximum vals
      spanh = maxh - minh
      compensate _ level = 10 * (level - minh) / spanh
  modify @HeightMap (mapWithKey compensate)

generateTerrain :: ValidSize n => Proxy n -> IO HeightMap
generateTerrain p = let hm = Data.HeightMap.empty p in
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
    let recur f item = local @(Rect Int) f (go *> item)
    let half x = x `div` 2
    rect <- ask @(Rect Int)
    when (rect^.width >= 2) $ do
      newside <- asks @(Rect Int) (^.width.to half)
      setEdges >>= setCenter
      local @Spread (/ 2.0)
        . recur (size.mapped %~ half)
        . recur (Rect.origin.xPos +~ newside)
        . recur (Rect.origin.yPos +~ newside)
        . recur (Rect.origin.xPos -~ newside)
        $ pure ()
