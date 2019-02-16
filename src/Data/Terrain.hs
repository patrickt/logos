{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Data.Terrain
  ( Terrain (Terrain)
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
      Forest -> Attr.rgbColor @Int 0   122 8
      Plains -> Attr.rgbColor @Int 0   237 35
      Desert -> Attr.rgbColor @Int 183 180 59
      Swamps -> Attr.rgbColor @Int 50  51  52

data Terrain = Terrain
  { _height :: !Double
  , _biome  :: !Biome
  } deriving (Show, Eq)

makeLenses ''Terrain

flood :: Terrain -> Terrain
flood t
  | t^.height < 3.5 = t & biome .~ Ocean
  | otherwise       = t & biome .~ Plains

instance CharDisplay Terrain where
  displayChar = displayChar . view biome
  displayAttr a = displayAttr a . view biome

instance CharDisplay (Elevation Terrain) where
  displayChar (Elevation t)
    | t^.height > 10 = 'X'
    | otherwise = head . show @Int . round . view height $ t

  displayAttr a (Elevation t) = a `Attr.withForeColor` color where
    h = t^.height
    color
      | h < 2     = Attr.rgbColor @Int 0   122 8
      | h < 5     = Attr.rgbColor @Int 0   237 35
      | h < 8     = Attr.rgbColor @Int 183 180 59
      | otherwise = Attr.rgbColor @Int 50  51  52
