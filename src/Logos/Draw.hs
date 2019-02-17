{-# LANGUAGE OverloadedLists #-}

module Logos.Draw (draw) where

import           Brick as Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Control.Effect
import           Control.Effect.Reader
import           Control.Lens hiding (views)
import           Control.Effect.Lens
import           Data.Version
import qualified Graphics.Vty as Vty

import           Data.Point
import           Data.Size
import qualified Data.Terrain as Terrain
import           Data.World hiding (center)
import           Logos.State (world, sidebar)
import qualified Logos.State as Logos
import           Paths_logos

versionLabel :: Widget n
versionLabel = str ("logos v" <> showVersion version)

draw :: Logos.State -> [Widget ()]
draw s = run . runReader s $ do
  display <- readout
  bar <- pure statusBar
  info <- views sidebar information
  let mainScreen = hBox [ keyBindings
                        , vBorder
                        , center display
                        , vBorder
                        , info
                        ]
  pure $ [borderWithLabel versionLabel (mainScreen <=> hBorder <=> bar)]

statusBar :: Widget n
statusBar = vLimit 1 . center $ str "Status bar"

readout :: ( Member (Reader Logos.State) sig
           , Carrier sig m, Monad m
           )
        => m (Widget n)
readout = do
  state <- ask
  let idx = state^.sidebar.listSelectedL.non 0
  let Size w h = dimensions (state^.world)
  pure . Widget Fixed Fixed . render . Brick.vBox $ do
    row <- [0..(h - 1)]
    pure $ Brick.hBox $ do
      col <- [0..(w - 1)]
      let res = lookupUnsafe (Point row col) (state^.world)
      pure . raw $ case idx of
        0 -> Terrain.charImage res
        1 -> Terrain.charImage (Terrain.Elevation res)
        _ -> Terrain.charImage (Terrain.Temperature res)

keyBindings :: Widget n
keyBindings = hLimit 30 . center $ vBox [ str "<r> Regenerate"
                                        , str "<s> Step"
                                        , str "<f> Flood world"
                                        , str "<q> Quit"
                                        ]

information :: (Monoid n, Show n, Ord n) => List n String -> Widget n
information
  = hLimit 30
  . center
  . renderList drawSelected True

drawSelected :: Bool -> String -> Widget n
drawSelected False s = str s
drawSelected True s  = selecting `Brick.modifyDefAttr` str s
  where selecting a = a
          `Vty.withForeColor` Vty.black
          `Vty.withBackColor` Vty.white
