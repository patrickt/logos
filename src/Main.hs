{-# LANGUAGE LambdaCase #-}

module Main where

import           Brick as Brick
import qualified Brick.BorderMap as BorderMap
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Data.Version
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Attr
import qualified Graphics.Vty.Image as Image
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Square
import qualified Math.Geometry.GridMap as GM
import           Math.Geometry.GridMap ((!))
import           Math.Geometry.GridMap.Lazy
import           Paths_logos (version)
import           Noise as Noise

data Biome = Ocean | Forest | Plains | Desert | Swamps deriving (Eq, Show)

data Terrain = Terrain
  { terrHeight :: !Double
  , terrBiome  :: !Biome
  } deriving (Show, Eq)

terrainChar :: Terrain -> Char
terrainChar t = case terrBiome t of
  Ocean  -> '~'
  Forest -> 'T'
  Plains -> '.'
  Desert -> '#'
  Swamps -> '%'

terrainElevation :: Terrain -> Char
terrainElevation t
  | terrHeight t > 10 = 'X'
  | otherwise = head . show . round . terrHeight $ t

terrainColor :: Terrain -> Vty.Attr
terrainColor t = Attr.defAttr `Attr.withForeColor` color where
  color = case terrBiome t of
    Ocean  -> Attr.blue
    Forest -> Attr.rgbColor 0   122 8
    Plains -> Attr.rgbColor 0   237 35
    Desert -> Attr.rgbColor 183 180 59
    Swamps -> Attr.rgbColor 50  51  52

terrainBurn :: Terrain -> Vty.Attr
terrainBurn t = Attr.defAttr `Attr.withForeColor` color where
  color
    | terrHeight t < 2 = Attr.rgbColor 0   122 8
    | terrHeight t < 5 = Attr.rgbColor 0   237 35
    | terrHeight t < 8 = Attr.rgbColor 183 180 59
    | otherwise      = Attr.rgbColor 50 51 52

terrainImage :: Terrain -> Vty.Image
terrainImage t = Image.char (terrainBurn t) (terrainElevation t)

newtype World = World
  { worldMap :: LGridMap RectSquareGrid Terrain
  } deriving (Show, Eq)

sampleWorld :: World
sampleWorld = World (lazyGridMap (rectSquareGrid 30 40) (repeat (Terrain 0 Plains)))

gennedWorld :: World -> World
gennedWorld w = w { worldMap = GM.mapWithKey go (worldMap w)} where
  go idx t = t { terrHeight = noise sampleParams idx (terrHeight t)}

worldImage :: World -> Widget ()
worldImage world = Widget Fixed Fixed . render . Brick.vBox $ do
  let (h, w) = size (worldMap world)
  -- let h = 3
  -- let w = 5
  row <- [0..(h - 1)]
  pure $ Brick.hBox $ do
    col <- [0..(w - 1)]
    let res = (worldMap world) ! (col, row)
    pure . raw . terrainImage $ res
  -- pure . Brick.hBox $ do
  --   col <- [0..6]
  --   pure $ Widget (terrainImage (worldMap w ! (row, col))) [] [] [] BorderMap.empty


versionLabel :: Widget ()
versionLabel = str ("logos v" <> showVersion version)

ui :: World -> Widget ()
ui w = borderWithLabel versionLabel (center (worldImage w))

main :: IO ()
main = do
  let world = gennedWorld sampleWorld
  simpleMain (ui world)
