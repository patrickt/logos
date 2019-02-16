{-# LANGUAGE KindSignatures, QuantifiedConstraints, ConstraintKinds, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}

module Data.HeightMap
  ( HeightMap (..)
  , ValidSize
  , empty
  , lookup
  , insert
  , makeHeightMap
  ) where

import Prelude hiding (lookup)

import           Control.Lens hiding (view)
import           Control.Monad
import           Control.Monad.Fail
import           Data.Coerce
import           Data.Maybe
import           Data.Proxy
import           Control.Effect
import           Control.Effect.Lens
import           Control.Effect.State
import           Control.Effect.Reader
import           Control.Effect.Random
import           Debug.Trace
import           GHC.TypeLits
import           Data.Rect (Rect, Point (Point), size, width, origin, xpos, ypos)
import qualified Data.Rect as Rect
import qualified Math.Geometry.Grid as G
import           Math.Geometry.Grid.Square
import qualified Math.Geometry.GridMap as GM
import qualified Math.Geometry.GridMap.Lazy as GM

newtype HeightMap = HeightMap
  { unHeightMap :: GM.LGridMap RectSquareGrid Double
  } deriving (Eq, Show)

class KnownNat n => ValidSize (n :: Nat)
instance ValidSize 3
instance ValidSize 5
instance ValidSize 9
instance ValidSize 17
instance ValidSize 33
instance ValidSize 65
instance ValidSize 129
instance ValidSize 257

data Params = Params
  { _spread :: Double
  } deriving (Eq, Show)

makeLenses ''Params

empty :: forall n . ValidSize n => Proxy n -> HeightMap
empty n = let size = fromIntegral $ natVal n
  in HeightMap (GM.lazyGridMap (rectSquareGrid size size) (repeat 0))

insert :: Point Int -> Double -> HeightMap -> HeightMap
insert (Point x y) d (HeightMap m) = HeightMap (GM.insert (x, y) d m)

side :: HeightMap -> Int
side (HeightMap m) = fst $ G.size m

lookup :: Point Int -> HeightMap -> Maybe Double
lookup (Point x y) (HeightMap h) = GM.lookup (x, y) h

values :: HeightMap -> [Double]
values (HeightMap m) = snd <$> GM.toList m

type Displacement sig m =
  ( Member (State HeightMap) sig
  , Member (Reader (Rect Int)) sig
  , Member (Reader Params) sig
  , Carrier sig m, MonadRandom m
  )

initializeCorners :: Displacement sig m => m [Double]
initializeCorners = do
  bounds <- asks Rect.corners
  forM bounds $ \point -> do
    rand <- getRandom
    rand <$ modify (insert point rand)

jitter :: Displacement sig m => Double -> m Double
jitter val = do
  munge <- view spread
  rand  <- getRandom
  let aroundZero = (munge * rand * 2) - munge
  pure (val + aroundZero)

setEdges :: Displacement sig m => [Double] -> m [Double]
setEdges [tl, tr, br, bl] = do
  let top    = avg tl tr
      right  = avg tr br
      bottom = avg bl br
      left   = avg tl bl
      avg a b = (a + b) / 2.0

  bounds <- fmap fromIntegral <$> ask @(Rect Int)
  let vals = zip [top, right, bottom, left] (Rect.edges bounds)
  forM vals $ \(avg, pos) -> do
    jittered <- jitter avg
    jittered <$ modify (insert (fmap ceiling pos) jittered)

setCenter :: Displacement sig m => [Double] -> m ()
setCenter vals = do
  let avg = sum vals / fromIntegral (length vals)
  jittered <- jitter avg
  rect <- ask @(Rect Int)
  let cent = (fmap fromIntegral rect) ^. Rect.center
  modify (insert (fmap round cent) jittered)


normalize :: Displacement sig m => m ()
normalize = do
  vals <- gets values
  let min, max, span :: Double
      min = minimum vals
      max = maximum vals
      span = max - min
      compensate _ level = (10 * (level - min) / span)
  modify (\(HeightMap hm) -> HeightMap (GM.mapWithKey compensate hm))

makeHeightMap :: forall n . (ValidSize n) => Proxy n -> IO HeightMap
makeHeightMap p = let hm = empty p in
  runM
  . evalRandomIO
  . execState hm
  . runReader (Params 0.3)
  . runReader (Rect.rect 0 0 (side hm - 1) (side hm - 1))
  $ churn

churn :: ( Member (State HeightMap) sig, Member (Reader (Rect Int)) sig, Member (Reader Params) sig, Carrier sig m, MonadRandom m) => m ()
churn = do
  initializeCorners >>= setEdges >>= setCenter
  rect <- ask @(Rect Int)
  if (rect^.width <= 2)
    then normalize
    else do
      let newside = rect^.width `div` 2
      let q1 = rect & size.mapped %~ (`div` 2)
      local (const q1) churn
      let q2 = q1 & origin.xpos +~ newside
      local (const q2) churn
      let q3 = q2 & origin.ypos +~ newside
      local (const q3) churn
      let q4 = q3 & origin.xpos -~ newside
      local (const q4) churn

-- average :: [Double] -> Double
-- average [] = 0
-- average xs = sum xs / fromIntegral (length xs)

-- corners :: ValidSize n => HeightMap n -> [(Int, Int)]
-- corners hm = [(0, 0), (0, s), (s, s), (s, 0)] where s = side hm

-- initializeCorners :: (ValidSize n, MonadRandom m) => HeightMap n -> m ([Double], HeightMap n)
-- initializeCorners m = do
--   rands <- replicateM 4 getRandom
--   let done = foldr (uncurry insert) m (zip (corners m) rands)
--   pure (rands, done)

-- data DisplaceParams = DisplaceParams
--   { lx     :: Int
--   , rx     :: Int
--   , by     :: Int
--   , ty     :: Int
--   , spread :: Double
--   } deriving (Show, Eq)

-- randAroundZero :: (MonadRandom m, Random n, Num n) => n -> m n
-- randAroundZero spread = do
--   x <- getRandom
--   pure ((20 * x * spread) - spread)

-- jitter :: (MonadRandom m, Random n, Num n) => n -> n -> m n
-- jitter spread value = (+ value) <$> getRandom

-- makeHeightMap :: (ValidSize n, MonadFail m, MonadRandom m) => m (HeightMap n)
-- makeHeightMap = do
--   (_, hm) <- initializeCorners empty
--   let params = DisplaceParams 0 (side hm) 0 (side hm) 0.1
--   displaceRecursively params hm

-- displace :: forall m n . (ValidSize n, MonadFail m, MonadRandom m) => DisplaceParams -> HeightMap n -> m (HeightMap n)
-- displace DisplaceParams{..} hm' = do
--   ([bottomLeft, bottomRight, topLeft, topRight], hm) <- initializeCorners hm'
--   let cx = roundUp . average . fmap fromIntegral $ [lx, rx]
--       cy = roundUp . average . fmap fromIntegral $ [by, ty]

--   let top = average [topLeft, topRight]
--       left = average [bottomLeft, topLeft]
--       bottom = average [bottomLeft, bottomRight]
--       right = average [bottomRight, topRight]

--   jbottom <- jitter bottom spread
--   jtop    <- jitter top spread
--   jleft   <- jitter left spread
--   jright  <- jitter right spread

--   let avg = average [jbottom, jtop, jleft, jright]
--   let [center] = G.centre . unHeightMap $ hm
--   jcenter <- jitter avg spread

--   let updates = [(cx, by, jbottom), (cx, ty, jtop), (lx, cy, jleft), (rx, cy, jright)]
--   let res = foldr (\(row, col, h) x -> insert (row, col) h x) hm updates

--   pure $ insert center jcenter res

-- roundUp :: Double -> Int
-- roundUp x = let base = x - fromIntegral (floor x) in (if base < 0.5 then round else ceiling) x

-- displaceRecursively :: forall m n . (ValidSize n, MonadFail m, MonadRandom m)
--                     => DisplaceParams
--                     -> HeightMap n
--                     -> m (HeightMap n)
-- displaceRecursively ps@DisplaceParams{..} h = do
--   let chunk = ((rx - lx) `div` 2) + 1
--   let a = ps { rx = rx - chunk, ty = ty - chunk}
--       b = ps { lx = lx + chunk, by = ty + chunk}
--       c = ps { rx = lx + chunk, ty = ty - chunk}
--       d = ps { lx = lx + chunk, ty = ty - chunk}

--   traceShowM (a, b)
--   h' <- displace ps h

--   if chunk < 3 then pure h' else do
--     displaceRecursively a h'
--       >>= displaceRecursively b
--       >>= displaceRecursively c

-- -- displaceRecursively 3 ps h = displace ps h
-- -- displaceRecursively n ps@DisplaceParams{..} h = do
-- --   let mid = round ((sum $ fmap fromIntegral [lx, rx]) / 2)
-- --   pure h
-- --     >>= displaceRecursively (n - 1) (ps { by = by - mid, lx = lx + mid})
-- --     >>= displaceRecursively (n - 1) (ps { by = by - mid, rx = rx - mid})
-- --     >>= displaceRecursively (n - 1) (ps { ty = ty + mid, rx = rx - mid})
-- --     >>= displaceRecursively (n - 1) (ps { ty = ty + mid, lx = lx + mid})
-- --   -- >>= displace (ps { by = by  mid, ry = ry - mid})
-- --   -- >>= displace (ps { rx = rx - mid, ry = ry - mid})

-- -- -- displaceRecursively params hm = do
-- -- --   let

-- -- -- todo iterate
