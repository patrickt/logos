module Logos.Draw (draw) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens
import Data.Version
import Control.Effect
import Control.Effect.Reader

import           Data.Point
import           Data.Size
import qualified Data.Terrain as Terrain
import           Data.World
import           Logos.State (world)
import qualified Logos.State as Logos
import           Paths_logos

versionLabel :: Widget n
versionLabel = str ("logos v" <> showVersion version)

draw :: Logos.State -> [Widget n]
draw s = run . runReader s $ do
  display <- readout
  let mainScreen = hBox [ keyBindings
                        , vBorder
                        , center display
                        , vBorder
                        , information
                        ]
  pure $ [borderWithLabel versionLabel mainScreen]


readout :: ( Member (Reader Logos.State) sig
           , Carrier sig m, Monad m
           )
        => m (Widget n)
readout = do
  state <- ask
  let Size w h = dimensions (state^.world)
  pure . Widget Fixed Fixed . render . Brick.vBox $ do
    row <- [0..(h - 1)]
    pure $ Brick.hBox $ do
      col <- [0..(w - 1)]
      let res = lookupUnsafe (Point row col) (state^.world)
      pure . raw . Terrain.charImage $ res

keyBindings :: Widget n
keyBindings = hLimit 30 . center $ vBox [ str "<r> Regenerate"
                                        , str "<s> Step"
                                        , str "<q> Quit"
                                        ]

information :: Widget n
information = hLimit 30 . center $ vBox [ str "Displaying: terrain"
                                        , str "Color: terrain"
                                        ]
