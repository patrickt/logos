{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Data.World
  ( World (..)
  , fromHeightMap
  , drawWorld
  , freshWorld
  , deluge
  , lookupUnsafe
  , dimensions
  ) where

import           Brick as Brick hiding (Size)
import           Control.Lens
import           Data.HeightMap as HeightMap
import           Data.Maybe
import           Data.Point
import           Data.Proxy
import           Data.Size hiding (size)
import           Data.Terrain
import           GHC.Stack
import           GHC.TypeLits
import           Math.Geometry.Grid hiding (Size)
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
freshWorld p = World (lazyGridMap (rectSquareGrid n n) (repeat (Terrain 0 Plains)))
  where n = fromIntegral . natVal $ p

dimensions :: World -> Size Int
dimensions (World m) = Size w h where (h, w) = size m

deluge :: World -> World
deluge (World m) = World (GM.mapWithKey (\(x, y) f -> flood f) m)

lookupUnsafe :: HasCallStack => Point Int -> World -> Terrain
lookupUnsafe (Point x y) (World m) = m ! (y, x)

drawWorld :: World -> Widget ()
drawWorld world = Widget Fixed Fixed . render . Brick.vBox $ do
  let Size w h = dimensions world
  row <- [0..(h - 1)]
  pure $ Brick.hBox $ do
    col <- [0..(w - 1)]
    let res = (worldMap world) ! (col, row)
    pure . raw . charImage $ res
