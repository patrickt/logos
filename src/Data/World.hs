{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.World
  ( World (..)
  , fromHeightMap
  , freshWorld
  , deluge
  , center
  , distance
  , mapWithPosition
  , traverseWithPosition
  , lookupUnsafe
  , dimensions
  ) where

import           Control.Effect
import           Control.Effect.State
import           Data.Foldable
import           Data.Traversable
import           Control.Effect.Lift
import           Control.Lens
import           Control.Monad
import           Data.HeightMap as HeightMap
import           Data.Maybe
import           Data.Point
import           Data.Proxy
import           Data.Size hiding (size)
import           Data.Terrain
import           GHC.Stack
import           GHC.TypeLits
import qualified Math.Geometry.Grid as Grid
import           Math.Geometry.Grid.Square
import           Math.Geometry.GridMap ((!))
import qualified Math.Geometry.GridMap as GM
import           Math.Geometry.GridMap.Lazy

newtype World = World
  { worldMap :: LGridMap RectSquareGrid Terrain
  } deriving (Show, Eq)

fromHeightMap :: HeightMap -> World -> World
fromHeightMap hm (World w) = World (GM.mapWithKey (\(x, y) f -> f & Data.Terrain.height .~ fromMaybe 0 (HeightMap.lookup (Point x y) hm)) w)

freshWorld :: KnownNat n => Proxy n -> World
freshWorld p = World (lazyGridMap (rectSquareGrid n n) (repeat (Terrain 0 Plains 0)))
  where n = fromIntegral . natVal $ p

dimensions :: World -> Size Int
dimensions (World m) = Size w h where (h, w) = Grid.size m

center :: World -> Point Int
center (World w) = Point row col where (col, row) = head $ Grid.centre (GM.toGrid w)

distance :: World -> Point Int -> Point Int -> Int
distance (World w) a b = Grid.distance w (forGrid a) (forGrid b)

deluge :: World -> World
deluge = mapTerrain flood

lookupUnsafe :: HasCallStack => Point Int -> World -> Terrain
lookupUnsafe (Point x y) (World m) = m ! (y, x)

mapTerrain :: (Terrain -> Terrain) -> World -> World
mapTerrain f (World w) = World (fmap f w)

mapWithPosition :: (Point Int -> Terrain -> Terrain) -> World -> World
mapWithPosition f (World m) = World (GM.mapWithKey (\(col, row) t -> f (Point row col) t) m)

traverseWithPosition :: Monad m
                     => (Point Int -> Terrain -> m Terrain)
                     -> World
                     -> m World
traverseWithPosition f (World m) = runM . fmap World . execState m $
  for (GM.toList m) $ \(pos, terr) -> do
    item <- sendM (f (fromGrid pos) terr)
    modify @(LGridMap RectSquareGrid Terrain) (GM.insert pos item)
