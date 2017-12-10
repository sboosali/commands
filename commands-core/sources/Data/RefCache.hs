{- |

usage:

@
import qualified Data.RefCache as RefCache
import Data.RefCache (RefCache)
@

-}
module Data.RefCache
 ( RefCache
 , empty
 , newCache
 , insert
 , lookup
 , cacheByRef
 , traverseShared
 , cacheIOByRef
 , traverseSharedIO
 , cacheSTByRef
 , traverseSharedST
 ) where

import Data.RefCache.Internal
import Prelude                ()
