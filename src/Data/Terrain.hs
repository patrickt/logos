{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Data.Terrain
  ( Terrain (Terrain)
  , height
  , biome
  , temperature
  , flood
  , balance
  , lookup
  , Biome (..)
  , charImage
  , Elevation (..)
  , Temperature (..)
  ) where

import Control.Lens
import Control.Monad.Random
import Data.Point
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Attr
import qualified Graphics.Vty.Image as Image
import Debug.Trace

newtype Elevation a = Elevation a
newtype Temperature a = Temperature a

class CharDisplay a where
  displayChar :: a -> Char
  displayAttr :: Vty.Attr -> a -> Vty.Attr

charImage :: CharDisplay a => a -> Vty.Image
charImage t = Image.char (displayAttr Attr.defAttr t) (displayChar t)

data Biome = Ocean | Forest | Plains | Desert | Mountain deriving (Eq, Show)

instance CharDisplay Biome where
  displayChar = \case
    Ocean    -> '~'
    Forest   -> 'T'
    Plains   -> '.'
    Desert   -> '#'
    Mountain -> '^'

  displayAttr attr self = attr `Attr.withForeColor` color where
    color = case self of
      Ocean    -> Attr.blue
      Forest   -> Attr.rgbColor @Int 0   122 8
      Plains   -> Attr.rgbColor @Int 0   237 35
      Desert   -> Attr.rgbColor @Int 183 180 59
      Mountain -> Attr.rgbColor @Int 50  51  52

data Terrain = Terrain
  { _height      :: !Double
  , _biome       :: !Biome
  , _temperature :: Double
  } deriving (Show, Eq)

makeLenses ''Terrain

flood :: Terrain -> Terrain
flood t = set biome go t
  where
    go
      | t^.height < 3.5 = Ocean
      | t^.height > 7.5 = Mountain
      | otherwise       = Plains

balance :: MonadRandom m
        => Point Int
        -> Terrain
        -> m Terrain
balance _ t = do
  temp <- getRandom
  pure (t & temperature .~ (temp * 100))

instance CharDisplay Terrain where
  displayChar = displayChar . view biome
  displayAttr a = displayAttr a . view biome

instance CharDisplay (Elevation Terrain) where
  displayChar (Elevation t)
    | t^.height > 10 = 'X'
    | otherwise = head . show @Int . round . view height $ t

  displayAttr a (Elevation t) = displayAttr a t

instance CharDisplay (Temperature Terrain) where
  displayChar (Temperature r) = displayChar r
  displayAttr a (Temperature r) = a `Attr.withForeColor` color where
    t = r^.temperature
    color
      | t < 20    = Attr.rgbColor @Int 234 252 255
      | t < 40    = Attr.rgbColor @Int 255 245 211
      | t < 60    = Attr.rgbColor @Int 255 181 168
      | t < 80    = Attr.rgbColor @Int 255 114 89
      | otherwise = Attr.rgbColor @Int 255 62  28

  -- displayAttr a (Elevation t) = a `Attr.withForeColor` color where
  --   h = t^.height
  --   color
  --     | h < 2     = Attr.rgbColor @Int 0   122 8
  --     | h < 5     = Attr.rgbColor @Int 0   237 35
  --     | h < 8     = Attr.rgbColor @Int 183 180 59
  --     | otherwise = Attr.rgbColor @Int 50  51  52
