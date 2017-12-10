module Data.RefCache.Extra where

import Control.Exception     (evaluate)
import System.Mem.StableName


forceStableName
 :: a -- ^ strict
 -> IO (StableName a)
forceStableName x = evaluate x >> makeStableName x

