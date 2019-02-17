module Logos.Event (Event (..)) where

import qualified Graphics.Vty as Vty

data Event
  = Quit
  | Flood
  | Regen
  | Arrow Vty.Event
    deriving (Eq, Show, Ord)
