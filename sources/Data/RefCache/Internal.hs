
{-# LANGUAGE AutoDeriveTypeable, LambdaCase, RankNTypes       #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeFamilies #-}
module Data.RefCache.Internal
 ( module Data.RefCache.Internal
 , module Data.RefCache.Extra
 ) where

import           Data.RefCache.Extra

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import           Data.IORef
import qualified Data.List               as List
import           Prelude                 hiding (lookup)
import           System.Mem.StableName


{- |

the key type @k@ and the value type @v@ have a functional dependency on an "item type" @i@. this avoids inserting differently-type values into the same key, which would segfault from unsafeCoerce.

the value type @v@ has a functional dependency on the key type @k@.

is abstract.

-}
newtype RefCache k v = RefCache (IntMap [(StableName k, IORef v)])
-- TODO type role nominal RefCache

{- |
-}
empty :: RefCache k v
empty = RefCache IntMap.empty

newCache :: IO (IORef (RefCache k v))
newCache = newIORef empty

insert
 :: k -- ^ strict in the key
 -> v
 -> RefCache k v
 -> IO (StableName k, RefCache k v)
insert x y m = do
 k <- forceStableName x
 v <- newIORef y
 return (k, insertRef k v m)

{- |

-}
insertRef :: StableName k -> IORef v -> RefCache k v -> RefCache k v
insertRef k v (RefCache m) = RefCache(IntMap.insertWith (++) (hashStableName k) [(k, v)] m)

lookup
 :: k -- ^ strict in the key
 -> RefCache k v
 -> IO (Maybe v)
lookup x m = do
 k <- forceStableName x
 let v = lookupRef k m
 y <- readIORef `traverse` v
 return y

{- |

hashes with 'hashStableName', disambiguate with 'eqStableName' (via 'Eq').

-}
lookupRef :: StableName k -> RefCache k v -> Maybe (IORef v)
lookupRef k (RefCache m) = case (List.lookup (k) <=< IntMap.lookup (hashStableName k)) m of
 Nothing -> Nothing
 Just v -> Just v

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
cacheByRef f = cacheIOByRef (return.f)
-- cacheByRef f = do
--  c <- newCache
--  return$ \x -> do
--   readIORef c >>= lookup x >>= \case
--    Just y  -> do
--     return y
--    Nothing -> do
--     let y = f x
--     k <- forceStableName x
--     v <- newIORef y
--     _ <- atomicModifyIORef' c ((,()) . insertRef k v)
--     return y
-- {-# NOINLINE cacheByRef #-}

{- | like 'traverse', but preserves sharing.

when @m@ is pure (e.g. @'State' Int@) (i.e. can't observe sharing), or more generally the effect @u@ is pure, 'traverseShared' shoud coincide with 'traverse'. when @m@ is impure (e.g. @IO@), the effect @u@ can violate referential transparency.

for example, we can "poke some holes" with @let@, then "fill those holes" with a ref like 'newIORef'.

>>> (traverse readIORef . traverse (atomicModifyIORef (+1)) . traverseShared newIORef) [0,0,0]  -- unshared
[1,1,1]

>>> let x = 0
>>> (traverse readIORef . traverse (atomicModifyIORef (+1)) . traverseShared newIORef) [x,x,x]  -- shared
[3,3,3]

pure Haskell can't observe the sharing which distinguishes @let x = 0 in [x,x,x]@ from @[0,0,0]@. hence, 'traverseShared' is impure, returning from 'IO'.


recover input with @'Identity'@, like 'traverse's @identity@ law:

>>> getIdentity <$> traverseShared Identity [0,0,0]
[0,0,0]



recover sharing-observing graph with (\_ -> (Const <$> newUniqueReally)):

>>>


@traverseShared 'return'@ preserves sharing:

>>>


to keep the sharing "as you see it" in the source, call GHC with these options:

*

*

*



-}
traverseShared :: (Traversable t) => (a -> b) -> t a -> IO (t b)
traverseShared u t = do
 u' <- cacheByRef u
 traverse u' t

cacheIOByRef :: (a -> IO b) -> IO (a -> IO b)
cacheIOByRef f = do
 c <- newCache
 return$ \x -> do
  readIORef c >>= lookup x >>= \case
   Just y  -> do
    return y
   Nothing -> do
    -- print "Miss"
    y <- f x
    k <- forceStableName x
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . insertRef k v)
    return y
{-# NOINLINE cacheIOByRef #-}

traverseSharedIO
 :: forall t a b. (Traversable t)
 => (a -> IO b)
 ->     t a
 -> IO (t b)
traverseSharedIO u t = do
 u' <- cacheIOByRef u
 traverse u' t

-- doesn't share
-- traverseSharedIO u t = do
--  u' <- cacheByRef u
--  t' <- traverse u' t
--  traverse id t'


cacheSTByRef :: (a -> ST s b) -> IO (a -> IO b)
cacheSTByRef u = cacheIOByRef (unsafeSTToIO.u)

traverseSharedST :: (Traversable t) => (a -> ST s b) -> t a -> IO (t b)
traverseSharedST u t = do
 u' <- cacheSTByRef u
 traverse u' t

