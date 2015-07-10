{- |

usage:

@
import qualified Data.RefCache as RefCache
import Data.RefCache (RefCache)
@

-}
module Data.HRefCache
 ( HRefCache
 , Named(..)
 , cacheIOByName
 , empty
 , insert
 , lookup
 ) where

import           Data.HRefCache.Internal
import Prelude ()
