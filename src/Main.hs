module Main where

import Brick
import Control.Monad
import Data.Proxy
import Graphics.Vty
import Control.Concurrent
import Control.Concurrent.Async
import Brick.BChan

import qualified Logos.App as Logos
import           Logos.State (initialState)
import qualified Logos.Event as Logos

main :: IO ()
main = do
  let builder = mkVty defaultConfig
  vty <- builder
  ticker <- newBChan 1
  void . async . forever $ do
    threadDelay 1000000
    writeBChan ticker Logos.Flood
  void $ customMain vty builder (Just ticker) Logos.mainApp (initialState (Proxy @33))
