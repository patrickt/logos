module Logos.App where

import qualified Brick
import qualified Brick.AttrMap as Attr
import qualified Graphics.Vty as Vty
import qualified Logos.State as Logos
import qualified Logos.Draw as Draw

type Event = ()
type Resource = ()

mainApp :: Brick.App Logos.State Event Resource
mainApp = Brick.App
  { appDraw         = Draw.draw
  , appChooseCursor = \_ _ -> Nothing
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const (Attr.attrMap Vty.defAttr [])
  }

handleEvent :: Logos.State
            -> Brick.BrickEvent Resource Event
            -> Brick.EventM Resource (Brick.Next Logos.State)
handleEvent s e = case e of
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') _) -> Brick.halt s
  _ -> Brick.continue s
