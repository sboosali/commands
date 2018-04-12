module Data.RefCache.Extra
  ( module Data.RefCache.Extra
  -- , module Prelude
  ) where

import Control.Exception     (evaluate)
import System.Mem.StableName

import Prelude

forceStableName
 :: a -- ^ strict
 -> IO (StableName a)
forceStableName x = evaluate x >> makeStableName x

