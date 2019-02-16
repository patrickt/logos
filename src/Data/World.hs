module Data.World
  ( World (..)
  , fromHeightMap
  , drawWorld
  , sampleWorld
  ) where

import           Brick as Brick hiding (Size)
import qualified Brick.BorderMap as BorderMap
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Control.Lens
import           Data.HeightMap as HeightMap
import           Data.Maybe
import           Data.Proxy
import           Data.Point
import           Data.Size hiding (size)
import           Data.Rect
import           Data.Terrain
import           Data.Version
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Attr
import qualified Graphics.Vty.Image as Image
import           Math.Geometry.Grid hiding (Size)
import           Math.Geometry.Grid.Square
import           Math.Geometry.GridMap ((!))
import qualified Math.Geometry.GridMap as GM
import           Math.Geometry.GridMap.Lazy

newtype World = World
  { worldMap :: LGridMap RectSquareGrid Terrain
  } deriving (Show, Eq)

sampleWorld :: World
sampleWorld = World (lazyGridMap (rectSquareGrid 33 33) (repeat (Terrain 0 Plains)))

fromHeightMap :: HeightMap -> World -> World
fromHeightMap hm (World w) = World (GM.mapWithKey (\(x, y) f -> f & Data.Terrain.height .~ fromMaybe 0 (HeightMap.lookup (Point x y) hm)) w)

dimensions :: World -> Size Int
dimensions (World m) = Size w h where (h, w) = size m

drawWorld :: World -> Widget ()
drawWorld world = Widget Fixed Fixed . render . Brick.vBox $ do
  let Size w h = dimensions world
  row <- [0..(h - 1)]
  pure $ Brick.hBox $ do
    col <- [0..(w - 1)]
    let res = (worldMap world) ! (col, row)
    pure . raw . charImage $ res
