{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logos.App where

import qualified Brick
import qualified Brick.AttrMap as Attr
import qualified Brick.Widgets.List as List
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Data.Proxy
import qualified Graphics.Vty as Vty

import           Data.HeightMap
import           Data.Terrain
import           Data.World
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

parseEvent :: Brick.BrickEvent Resource Event -> Maybe Logos.Event
parseEvent = \case
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') _)   -> Just Logos.Quit
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'r') _)   -> Just Logos.Regen
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'f') _)   -> Just Logos.Flood
  Brick.VtyEvent k@(Vty.EvKey Vty.KUp         _) -> Just (Logos.Arrow k)
  Brick.VtyEvent k@(Vty.EvKey Vty.KDown       _) -> Just (Logos.Arrow k)
  _                                              -> Nothing

handleEvent :: Logos.State
            -> Brick.BrickEvent Resource Event
            -> Brick.EventM Resource (Brick.Next Logos.State)
handleEvent s e = case parseEvent e of
  Nothing              -> Brick.continue s
  Just Logos.Quit      -> Brick.halt s
  Just Logos.Flood     -> do
    withTemp <- traverseWithPosition balance (s^.world.to deluge)
    Brick.continue (s & world .~ withTemp)
  Just Logos.Regen     -> do
    hm <- liftIO . makeHeightMap $ Proxy @33
    let newState = s & world %~ fromHeightMap hm
    Brick.continue newState
  Just (Logos.Arrow vty) -> do
    newList <- List.handleListEvent vty (s^.sidebar)
    Brick.continue (s & sidebar .~ newList)
