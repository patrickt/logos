{-# LANGUAGE TemplateHaskell, OverloadedLists #-}

module Logos.State
  ( State (State)
  , initialState
  , mode
  , world
  , sidebar
  , Mode (..)
  ) where

import qualified Brick.Widgets.List as Brick
import           Control.Lens.TH
import           Data.Proxy
import           Data.World
import           GHC.TypeLits

data Mode
  = Fresh
  | Generated

data State = State
  { _mode    :: !Mode
  , _world   :: !World
  , _sidebar :: !(Brick.List () String)
  }

initialState :: KnownNat n => Proxy n -> State
initialState p = State
  { _mode  = Fresh
  , _world = freshWorld p
  , _sidebar = Brick.list mempty ["Terrain", "Elevation", "Temperature"] 1
  }

makeLenses ''State
