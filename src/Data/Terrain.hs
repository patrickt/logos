{-# LANGUAGE LambdaCase #-}

module Data.Terrain
  ( Terrain (..)
  , height
  , biome
  , flood
  , lookup
  , Biome (..)
  , charImage
  , Elevation (..)
  ) where

import Control.Lens
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Attributes as Attr
import qualified Graphics.Vty.Image as Image

newtype Elevation a = Elevation a

class CharDisplay a where
  displayChar :: a -> Char
  displayAttr :: Vty.Attr -> a -> Vty.Attr

charImage :: CharDisplay a => a -> Vty.Image
charImage t = Image.char (displayAttr Attr.defAttr t) (displayChar t)

data Biome = Ocean | Forest | Plains | Desert | Swamps deriving (Eq, Show)

instance CharDisplay Biome where
  displayChar = \case
    Ocean  -> '~'
    Forest -> 'T'
    Plains -> '.'
    Desert -> '#'
    Swamps -> '%'

  displayAttr attr self = attr `Attr.withForeColor` color where
    color = case self of
      Ocean  -> Attr.blue
      Forest -> Attr.rgbColor 0   122 8
      Plains -> Attr.rgbColor 0   237 35
      Desert -> Attr.rgbColor 183 180 59
      Swamps -> Attr.rgbColor 50  51  52

data Terrain = Terrain
  { terrainHeight :: !Double
  , terrainBiome  :: !Biome
  } deriving (Show, Eq)

height :: Lens' Terrain Double
height = lens terrainHeight (\t h -> t { terrainHeight = h})

biome :: Lens' Terrain Biome
biome = lens terrainBiome (\t h -> t { terrainBiome = h})

flood :: Terrain -> Terrain
flood t
  | t^.height < 1 = t & biome .~ Ocean
  | otherwise     = t

instance CharDisplay Terrain where
  displayChar = displayChar . Elevation
  displayAttr a = displayAttr a . view biome

instance CharDisplay (Elevation Terrain) where
  displayChar (Elevation t)
    | t^.height > 10 = 'X'
    | otherwise = head . show . round . view height $ t

  displayAttr a (Elevation t) = a `Attr.withForeColor` color where
    h = t^.height
    color
      | h < 2 = Attr.rgbColor 0   122 8
      | h < 5 = Attr.rgbColor 0   237 35
      | h < 8 = Attr.rgbColor 183 180 59
      | otherwise      = Attr.rgbColor 50 51 52
