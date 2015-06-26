{-# LANGUAGE RankNTypes, BangPatterns, LambdaCase, AutoDeriveTypeable, TypeFamilies #-}
{- |

usage:

@
import qualified Data.RefCache as RefCache
import Data.RefCache (RefCache)
@

-}
module Data.RefCache
 ( RefCache
 , cacheByRef
 , empty
 , insert
 , lookup
 ) where

import Data.IORef
import System.Mem.StableName
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.List as List
import Prelude hiding (lookup)
import Control.Monad ((<=<)) 


{- | 

the key type @k@ and the value type @v@ have a functional dependency on an "item type" @i@. this avoids inserting differently-type values into the same key, which would segfault from unsafeCoerce.

the value type @v@ has a functional dependency on the key type @k@.

is abstract.

-}
newtype RefCache k v = RefCache (IntMap [(StableName k, v)])
-- TODO type role nominal RefCache

{- | 
-}
empty :: RefCache k v
empty = RefCache IntMap.empty

insert :: k -> v -> RefCache k v -> IO (StableName k, RefCache k v)
insert !x v m = do
 k <- makeStableName x  -- the BangPattern has evaluated x to WHNF
 return$ (k, insertRef k v m)

{- | 

-}
insertRef :: StableName k -> v -> RefCache k v -> RefCache k v
insertRef k v (RefCache m) = RefCache(IntMap.insertWith (++) (hashStableName k) [(k, v)] m)

lookup :: k -> RefCache k v -> IO (Maybe v)
lookup !x m = do
 k <- makeStableName x  -- the BangPattern has evaluated x to WHNF
 return$ lookupRef k m

{- | 

hashes with 'hashStableName', disambiguate with 'eqStableName' (via 'Eq').

-}
lookupRef :: StableName k -> RefCache k v -> Maybe v
lookupRef k (RefCache m) = case (List.lookup (k) <=< IntMap.lookup (hashStableName k)) m of
 Nothing -> Nothing
 Just v -> Just (v)

{- | 

the cached function becomes strict in @a@.



@(\f -> 'unsafePerformIO' (cacheByRef f)) :: (a -> b) -> (a -> IO b)@ should be safe:

* when there's no let-floating

*


specializations:

@
cacheByRef ::             (a -> b -> c)  -> IO             (a -> IO (b -> c))
cacheByRef :: (forall x. f x -> m (g x)) -> IO (forall x. f x -> IO (m (g x)))
@

(uses an 'IORef'.)

related: Section 3 ("Benign Side Effects") in <http://community.haskell.org/~simonmar/papers/weak.pdf Stretching the Storage Manager: Weak Pointers an Stable Names in Haskell>

-}
cacheByRef :: (a -> b) -> IO (a -> IO b)
cacheByRef f = do
 cache <- newIORef empty
 return$ \(!x) -> do
  k <- makeStableName x  -- the BangPattern has evaluated x to WHNF
  lookupRef k <$> readIORef cache >>= \case
   Just y  -> return y
   Nothing -> do
    let y = f x
    atomicModifyIORef' cache (\c -> (insertRef k y c, ()))
    return y
{-# NOINLINE cacheByRef #-}

