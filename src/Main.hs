module Main where

import Brick
import Control.Monad
import Data.Proxy

import qualified Logos.App as Logos
import           Logos.State (initialState)

main :: IO ()
main = void $ defaultMain Logos.mainApp (initialState (Proxy @33))
