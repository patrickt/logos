module Logos.Event (Event (..)) where

data Event
  = Quit
  | Flood
  | Regen
    deriving (Eq, Show, Ord)
