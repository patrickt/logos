{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logos.App where

import qualified Brick
import qualified Brick.AttrMap as Attr
import qualified Brick.Widgets.List as List
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Data.List
import           Data.Proxy
import qualified Graphics.Vty as Vty
import Data.Ord

import           Data.HeightMap
import           Data.Point (distanceBetween, x, y)
import           Data.Size (width)
import           Data.Terrain
import           Data.World
import           Debug.Trace
import qualified Logos.Draw as Draw
import qualified Logos.Event as Logos
import           Logos.State (sidebar, world)
import qualified Logos.State as Logos

type Event = ()
type Resource = ()

instance MonadRandom (Brick.EventM r) where
  getRandomR  = liftIO . getRandomR
  getRandom   = liftIO getRandom
  getRandomRs = liftIO . getRandomRs
  getRandoms  = liftIO getRandoms

mainApp :: Brick.App Logos.State Event Resource
mainApp = Brick.App
  { appDraw         = Draw.draw
  , appChooseCursor = \_ _ -> Nothing
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const (Attr.attrMap Vty.defAttr [])
  }

(///) :: (Integral a, Integral b) => a -> b -> Double
a /// b = fromIntegral a / fromIntegral b

parseEvent :: Brick.BrickEvent Resource Event -> Maybe Logos.Event
parseEvent = \case
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') _)   -> Just Logos.Quit
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'r') _)   -> Just Logos.Regen
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'f') _)   -> Just Logos.Flood
  Brick.VtyEvent k@(Vty.EvKey Vty.KUp         _) -> Just (Logos.Arrow k)
  Brick.VtyEvent k@(Vty.EvKey Vty.KDown       _) -> Just (Logos.Arrow k)
  _                                              -> Nothing

onFlood :: MonadRandom m => World -> m World
onFlood x = do
  let w = deluge x
  let middle = center w
  let balance pos t = do
        let (ht :: Int) = Control.Lens.view width (dimensions w)
        let atTop = pos & y .~ 0
        let atBot = pos & y .~ ht
        let lower = minimumBy (comparing abs) [ distanceBetween pos atTop, distanceBetween pos atBot
                                              , distanceBetween atTop pos, distanceBetween atBot pos
                                              ]

        let offset = sin ((lower / (fromIntegral ht)) * (pi / 2))
        --traceShowM (pos, lower, ht, offset)
        temp <- fmap (/5) $ getRandom
        let adjusted = (offset + temp) * 100 --min 101 ((offset + temp) * 140)
        let newTemp = case t^.biome of
              Mountain -> temp * 20
              Ocean    -> adjusted * 0.7
              _        -> adjusted
        pure (t & temperature .~ newTemp)
  traverseWithPosition balance w

handleEvent :: Logos.State
            -> Brick.BrickEvent Resource Event
            -> Brick.EventM Resource (Brick.Next Logos.State)
handleEvent s e = case parseEvent e of
  Nothing              -> do
    hm <- liftIO . makeHeightMap $ Proxy @33
    done <- onFlood (fromHeightMap hm (s^.world))
    Brick.continue (s & world .~ done)
  Just Logos.Quit      -> Brick.halt s
  Just Logos.Flood     -> do
    x <- onFlood (s^.world)
    Brick.continue (s & world .~ x)
  Just Logos.Regen     -> do
    hm <- liftIO . makeHeightMap $ Proxy @33
    done <- onFlood (fromHeightMap hm (s^.world))
    Brick.continue (s & world .~ done)
  Just (Logos.Arrow vty) -> do
    newList <- List.handleListEvent vty (s^.sidebar)
    Brick.continue (s & sidebar .~ newList)
