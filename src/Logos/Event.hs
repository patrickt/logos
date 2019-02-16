module Logos.Event (Event (..)) where

data Event
  = Quit
  | Regen
    deriving (Eq, Show, Ord)
