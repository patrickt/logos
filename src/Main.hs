{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (lookup)

import           Brick as Brick
import qualified Brick.BorderMap as BorderMap
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Control.Lens
import           Data.Version
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Attr
import qualified Graphics.Vty.Image as Image
import Data.Maybe
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Square
import           Math.Geometry.GridMap ((!))
import qualified Math.Geometry.GridMap as GM
import           Math.Geometry.GridMap.Lazy
import           Paths_logos (version)
import Data.Rect (Point (Point))
import Data.Proxy

import Data.Terrain as Terrain
import Data.HeightMap as HeightMap
import Noise as Noise

newtype World = World
  { worldMap :: LGridMap RectSquareGrid Terrain
  } deriving (Show, Eq)

sampleWorld :: World
sampleWorld = World (lazyGridMap (rectSquareGrid 30 40) (repeat (Terrain 0 Plains)))

fromHeightMap :: HeightMap -> World -> World
fromHeightMap hm (World w) = World (GM.mapWithKey (\(x, y) f -> f & height .~ 11 * (fromMaybe 0 (HeightMap.lookup (Point x y) hm))) w)

gennedWorld :: World -> World
gennedWorld w = w { worldMap = GM.mapWithKey go (worldMap w)} where
  go idx = over height (noise sampleParams idx)

worldImage :: World -> Widget ()
worldImage world = Widget Fixed Fixed . render . Brick.vBox $ do
  let (h, w) = size (worldMap world)
  -- let h = 3
  -- let w = 5
  row <- [0..(h - 1)]
  pure $ Brick.hBox $ do
    col <- [0..(w - 1)]
    let res = (worldMap world) ! (col, row)
    pure . raw . charImage . Elevation $ res
  -- pure . Brick.hBox $ do
  --   col <- [0..6]
  --   pure $ Widget (terrainImage (worldMap w ! (row, col))) [] [] [] BorderMap.empty


versionLabel :: Widget ()
versionLabel = str ("logos v" <> showVersion version)

ui :: World -> Widget ()
ui w = borderWithLabel versionLabel (center (worldImage w))

main :: IO ()
main = do
  hm <- makeHeightMap (Proxy @33)
  let world = fromHeightMap hm sampleWorld
  simpleMain (ui world)
