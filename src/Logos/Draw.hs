module Logos.Draw (draw) where

import Brick
import Logos.State
import Data.Version
import Brick.Widgets.Border
import Brick.Widgets.Center
import Paths_logos

versionLabel :: Widget n
versionLabel = str ("logos v" <> showVersion version)

draw :: State -> [Widget n]
draw _ = [borderWithLabel versionLabel mainScreen] where
  mainScreen = hBox [ keyBindings
                    , vBorder
                    , center (str "screen")
                    , vBorder
                    , information
                    ]

keyBindings :: Widget n
keyBindings = hLimit 30 . center $ vBox [ str "<r> Regenerate"
                                        , str "<s> Step"
                                        , str "<q> Quit"
                                        ]

information :: Widget n
information = hLimit 30 . center $ vBox [ str "Displaying: terrain"
                                        , str "Color: terrain"
                                        ]
