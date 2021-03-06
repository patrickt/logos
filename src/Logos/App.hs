{-# LANGUAGE LambdaCase, PatternSynonyms, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logos.App where

import qualified Brick
import qualified Brick.AttrMap as Attr
import qualified Brick.Widgets.List as List
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Data.List
import           Data.Ord
import           Data.Proxy
import qualified Graphics.Vty as Vty

import           Data.Point (distanceBetween, yPos)
import           Data.Size (width)
import           Data.Terrain
import           Data.World
import qualified Logos.Draw as Draw
import qualified Logos.Event as Logos
import           Logos.State (sidebar, world)
import qualified Logos.State as Logos
import           Logos.Terrain

type Resource = ()

instance MonadRandom (Brick.EventM r) where
  getRandomR  = liftIO . getRandomR
  getRandom   = liftIO getRandom
  getRandomRs = liftIO . getRandomRs
  getRandoms  = liftIO getRandoms

mainApp :: Brick.App Logos.State Logos.Event Resource
mainApp = Brick.App
  { appDraw         = Draw.draw
  , appChooseCursor = \_ _ -> Nothing
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const (Attr.attrMap Vty.defAttr [])
  }

pattern Key :: Vty.Key -> [Vty.Modifier] -> Brick.BrickEvent n e
pattern Key a x = Brick.VtyEvent (Vty.EvKey a x)

parseEvent :: Brick.BrickEvent Resource Logos.Event -> Maybe Logos.Event
parseEvent = \case
  Key (Vty.KChar 'q') _                    -> Just Logos.Quit
  Key (Vty.KChar 'r') _                    -> Just Logos.Regen
  Key (Vty.KChar 'f') _                    -> Just Logos.Flood
  Key (Vty.KChar 'p') [Vty.MCtrl]          -> Just (Logos.Arrow (Vty.EvKey Vty.KUp []))
  Key (Vty.KChar 'n') [Vty.MCtrl]          -> Just (Logos.Arrow (Vty.EvKey Vty.KDown []))
  Brick.VtyEvent k@(Vty.EvKey Vty.KUp   _) -> Just (Logos.Arrow k)
  Brick.VtyEvent k@(Vty.EvKey Vty.KDown _) -> Just (Logos.Arrow k)
  Brick.AppEvent e                         -> Just e
  _                                        -> Nothing

onFlood :: MonadRandom m => World -> m World
onFlood x = do
  let w = deluge x
  let ht = w^.to dimensions.width
  let balance pos t = do
        jitter <- fmap (/5) $ getRandom
        let
          offset = sin ((lower / (fromIntegral ht)) * (pi / 2))
          atTop = pos & yPos .~ 0
          atBot = pos & yPos .~ ht
          lower = minimumBy (comparing abs) [ distanceBetween pos atTop
                                            , distanceBetween pos atBot
                                            , distanceBetween atTop pos
                                            , distanceBetween atBot pos
                                            ]
          adjusted = (offset + jitter) * 120
          newTemp = case t^.biome of
            Mountain -> jitter * 20
            Ocean    -> adjusted * 0.7
            _        -> adjusted

        pure (t & temperature .~ newTemp)
  traverseWithPosition balance w

handleEvent :: Logos.State
            -> Brick.BrickEvent Resource Logos.Event
            -> Brick.EventM Resource (Brick.Next Logos.State)
handleEvent s e = case parseEvent e of
  Nothing                -> Brick.continue s
  Just Logos.Quit        -> Brick.halt s
  Just Logos.Flood       -> world onFlood s >>= Brick.continue
  Just (Logos.Arrow vty) -> sidebar (List.handleListEvent vty) s >>= Brick.continue
  Just Logos.Regen       -> do
    hm <- liftIO . generateTerrain $ Proxy @33
    world (onFlood . fromHeightMap hm) s >>= Brick.continue
