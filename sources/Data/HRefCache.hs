{-# LANGUAGE AutoDeriveTypeable, LambdaCase, RankNTypes, TupleSections #-}
{-# LANGUAGE TypeFamilies, TypeOperators                               #-}
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

import           Data.RefCache.Etc

import           Control.Arrow         ((>>>))
import           Control.Monad         ((<=<))
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IntMap
import           Data.IORef
import qualified Data.List             as List
import           GHC.Prim              (Any)
import           Prelude               hiding (lookup)
import           System.Mem.StableName
import           Unsafe.Coerce


{- | a @H@eterogeneous store that @Cache@s by @Ref@erence.

"heterogeneous" because it has key type @(f Any)@ and the value type @(g Any)@.

-}
newtype HRefCache f g = HRefCache (IntMap [(StableName (f Any), IORef (g Any))])

empty :: HRefCache f g
empty = HRefCache IntMap.empty

newCache :: IO (IORef (HRefCache f g))
newCache = newIORef empty

insert
 :: f a -- ^ strict in the key
 -> g a
 -> HRefCache f g
 -> IO (StableName (f a), HRefCache f g)
insert x y m = do
 k <- forceStableName x
 v <- newIORef y
 return (k, insertRef k v m)

lookup
 :: f a -- ^ strict in the key
 -> HRefCache f g
 -> IO (Maybe (g a))
lookup x m = do
 k <- forceStableName x
 y <- readIORef `traverse` lookupRef k m
 return y

{- |

calls @('unsafeCoerce' :: StableName (f a) -> StableName 'Any')@ to "homogenize" the key
and @('unsafeCoerce' :: (g a) -> (g 'Any'))@ to "homogenize" the value.

see <http://hackage.haskell.org/package/ghc-prim-0.4.0.0/docs/GHC-Prim.html#v:unsafeCoerce-35- unsafeCoerce#>.

-}
insertRef :: StableName (f a) -> IORef (g a) -> HRefCache f g -> HRefCache f g
insertRef k v (HRefCache m) = HRefCache(IntMap.insertWith (++) (hashStableName k) [(unsafeCoerce k, unsafeCoerce v)] m)

{- |

hashes with 'hashStableName', disambiguate with 'eqStableName' (via 'Eq').

calls @('unsafeCoerce' :: StableName (f a) -> StableName (f 'Any'))@.
calls @('unsafeCoerce' :: IORef (g 'Any') -> IORef (g a))@ to recover the type of the value. the phantom parameters of 'StableName' and 'HRefCache', in 'lookupRef' and 'insertRef', prove this cast's safety (since the @a@'s are the same).

see <http://hackage.haskell.org/package/ghc-prim-0.4.0.0/docs/GHC-Prim.html#v:unsafeCoerce-35- unsafeCoerce#>.

-}
lookupRef :: StableName (f a) -> HRefCache f g -> Maybe (IORef (g a))
lookupRef k (HRefCache m) = case (List.lookup (unsafeCoerce k) <=< IntMap.lookup (hashStableName k)) m of
 Nothing -> Nothing
 Just v -> Just (unsafeCoerce v)


{- | -}
class Named t where
 type NameOf t :: * -> *
 type KeyOf  t :: * -> *
 rename
  :: (Applicative m)
  => (forall x. KeyOf t x -> NameOf t x -> m (NameOf t' x))
  -> (t a -> m (t' a))

cacheIOByName
 :: (Named t)
 => (forall x. KeyOf t x -> NameOf t x -> IO (NameOf t' x))
 -> IO (t a -> IO (t' a))
cacheIOByName u = do
 c <- newCache                  -- :: IORef (HRefCache t (NameOf t'))
 return$ rename$ \t x -> do
  k <- forceStableName t
  readIORef c >>= (lookupRef k >>> traverse readIORef) >>= \case
   Just y  -> return y
   Nothing -> do
    y <- u t x
    v <- newIORef y
    _ <- atomicModifyIORef' c ((,()) . insertRef k v)
    return y

