{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (lookup)

import           Brick as Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Control.Monad
import           Data.Proxy
import           Data.Size
import           Data.Version
import           Data.World
import qualified Logos.App as Logos
import           Logos.State (initialState)
import           Math.Geometry.GridMap ((!))
import           Paths_logos (version)

import Data.Terrain as Terrain

worldImage :: World -> Widget ()
worldImage world = Widget Fixed Fixed . render . Brick.vBox $ do
  let Size w h = dimensions world
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
  void $ defaultMain Logos.mainApp (initialState (Proxy @17))
  -- hm <- makeHeightMap (Proxy @17)
  -- let (World dry) = fromHeightMap hm (freshWorld (Proxy @17))
  -- let wet = World (fmap Terrain.flood dry)
  -- simpleMain (ui wet)
