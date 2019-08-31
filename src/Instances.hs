{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Control.Monad.Trans.Class
import Control.Effect.Reader
import Control.Monad.Random
import Control.Effect.State.Strict

instance (MonadRandom m) => MonadRandom (StateC s m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance (MonadRandom m) => MonadRandom (ReaderC r m) where
  getRandom = lift getRandom
  getRandomR = lift . getRandomR
  getRandoms = lift getRandoms
  getRandomRs = lift . getRandomRs
