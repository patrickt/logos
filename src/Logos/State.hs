{-# LANGUAGE TemplateHaskell #-}

module Logos.State
  ( State (State)
  , initialState
  , mode
  , world
  , Mode (..)
  ) where

import Control.Lens.TH
import Data.World
import GHC.TypeLits
import Data.Proxy

data Mode
  = Fresh
  | Generated

data State = State
  { _mode  :: !Mode
  , _world :: !World
  }

initialState :: KnownNat n => Proxy n -> State
initialState = State Fresh . freshWorld

makeLenses ''State
