module Noise where

import           Control.Monad
import           Control.Monad.ST.Safe
import           Data.Maybe
import           Data.STRef
import           Debug.Trace
import           Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range
import           Math.Noise as Noise
import           Math.Noise.Modules.Perlin (perlin)

data Params = Params
  { baseAmount  :: Double
  , octaves     :: Int
  , persistence :: Double
  , initialFreq :: Double
  , interval    :: Range Double
  }

sampleParams :: Params
sampleParams = Params 100 8 0.03 2000 (Range.constant 0 10)

noise :: Params -> (Int, Int) -> Double -> Double
noise Params{..} (row, col) height = runST $ do
  let ref    = newSTRef
      a += b = modifySTRef a (+ b)
      a *= b = modifySTRef a (* b)

  total        <- ref 0.0
  maxAmplitude <- ref 0.0
  amplitude    <- ref 1.0
  frequency    <- ref initialFreq
  pure . (*100) . abs . fromMaybe 0 $ Noise.getValue perlin (fromIntegral row, fromIntegral col, 0.012)
  --   traceShowM noise

  --   total        += noise
  --   frequency    *= 2
  --   maxAmplitude += amp
  --   amplitude    *= persistence

  -- tot <- readSTRef total
  -- amp <- readSTRef amplitude
  -- traceShowM (tot, amp)
  -- pure (height + (tot / amp))
