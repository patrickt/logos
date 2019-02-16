{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (lookup)

import           Brick as Brick
import qualified Brick.BorderMap as BorderMap
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Control.Lens
import           Data.Maybe
import           Data.Proxy
import           Data.Rect hiding (center)
import           Data.Version
import           Data.World
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Attr
import qualified Graphics.Vty.Image as Image
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Square
import           Math.Geometry.GridMap ((!))
import qualified Math.Geometry.GridMap as GM
import           Math.Geometry.GridMap.Lazy
import           Paths_logos (version)

import Data.HeightMap as HeightMap
import Data.Terrain as Terrain
import Noise as Noise

worldImage :: World -> Widget ()
worldImage world = Widget Fixed Fixed . render . Brick.vBox $ do
  let (h, w) = size (worldMap world)
  -- let h = 3
  -- let w = 5
  row <- [0..(h - 1)]
  pure $ Brick.hBox $ do
    col <- [0..(w - 1)]
    let res = (worldMap world) ! (col, row)
    pure . raw . charImage $ res
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
  let (World dry) = fromHeightMap hm sampleWorld
  let wet = World (fmap Terrain.flood dry)
  simpleMain (ui wet)
