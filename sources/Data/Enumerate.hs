{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DefaultSignatures, TypeOperators, FlexibleInstances, FlexibleContexts, DeriveAnyClass, RankNTypes #-}

{- | see 'Enumerable'.  

related:

http://hackage.haskell.org/package/emgm-0.4/docs/Generics-EMGM-Functions-Enum.html
allows infinite lists (by convention)
too heavyweight

http://hackage.haskell.org/package/enumerable
no Generic

https://hackage.haskell.org/package/testing-feat-0.4.0.2/docs/Test-Feat-Class.html#t:Enumerable
too heavyweight

https://hackage.haskell.org/package/smallcheck
too heavyweight
Series enumerates up to some depth and can enumerated infinitely-inhabited types

https://hackage.haskell.org/package/quickcheck
too heavyweight
randomness unnecessary 

http://www.haskell.org/haskellwiki/GHC.Generics

https://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-Generics.html

-}

module Data.Enumerate where
import           Commands.Etc

import Control.Monad.Catch

import           GHC.Generics
import           Data.Proxy
-- import qualified Data.Map as Map
-- import           Data.Map (Map)
import           Control.Arrow ((&&&))
import           Data.List (genericLength)
import           Data.Void (Void)
import           Data.Word (Word8, Word16)
import           Data.Int (Int8, Int16)
import Control.Exception (ErrorCall(..))


{- | enumerate the set of (depth first) all values in a (finitely enumerable) type.

generalizes Enums to any finite/discrete type. an Enumerable is either:

* an Enum
* a product of Enumerables
* a sum of Enumerables

can be implemented automatically via a 'Generic' instance.

laws:

* consistent:

    * @'cardinality' = 'length' 'enumerated'@

    so you can index the 'enumerated' with a nonnegative index below the 'cardinality'.

* distinct:

    * @(Eq a) => 'nub' 'enumerated' == 'enumerated'@

* complete:

    * @x `'elem'` 'enumerated'@

* coincides with @Bounded@ @Enum@s:

    * @('Enum' a, 'Bounded' a) => 'enumerated' == 'boundedEnumerated'@

    * @('Enum' a) => 'enumerated' == 'enumEnumerated'@

(@Bounded@ constraint elided for convenience, but relevant.)

("inputs" a type, outputs a list of values).

-}
class Enumerable a where

 enumerated :: [a]

 default enumerated :: (Generic a, GEnumerable (Rep a)) => [a]
 enumerated = to <$> genumerated

 cardinality :: proxy a -> Integer
 cardinality _ = genericLength (enumerated :: [a]) 
 -- overrideable for performance, but don't lie!

 -- default cardinality :: (Generic a, GEnumerable (Rep a)) => proxy a -> Integer
 -- cardinality _ = gcardinality (Proxy :: Proxy (Rep a))
 -- TODO merge both methods into one that returns their pair

-- | "Generic Enumerable", lifted to unary type constructors.
class GEnumerable f where
 genumerated :: [f x]
 gcardinality :: proxy f -> Integer

-- | empty list 
instance GEnumerable (V1) where
 genumerated = []
 gcardinality _ = 0

-- | singleton list 
instance GEnumerable (U1) where
 genumerated = [U1]
 gcardinality _ = 1

-- | call 'enumerated'
instance (Enumerable a) => GEnumerable (K1 i a) where
 genumerated = K1 <$> enumerated
 gcardinality _ = cardinality (Proxy :: Proxy a)

-- | multiply lists with @concatMap@
instance (GEnumerable (f), GEnumerable (g)) => GEnumerable (f :*: g) where
 genumerated =  (:*:) <$> genumerated <*> genumerated
 gcardinality _ = gcardinality (Proxy :: Proxy (f)) * gcardinality (Proxy :: Proxy (g))

-- | add lists with @(<>)@
instance (GEnumerable (f), GEnumerable (g)) => GEnumerable (f :+: g) where
 genumerated = map L1 genumerated ++ map R1 genumerated 
 gcardinality _ = gcardinality (Proxy :: Proxy (f)) + gcardinality (Proxy :: Proxy (g))

-- | ignore metadata
instance (GEnumerable (f)) => GEnumerable (M1 i t f) where
 genumerated = M1 <$> genumerated
 gcardinality _ = gcardinality (Proxy :: Proxy (f))

-- base types 
instance Enumerable Void
instance Enumerable ()
instance Enumerable Bool
instance Enumerable Ordering
{- | 

>>> (maxBound::Word8) - (minBound::Word8)
256

-}
instance Enumerable Int8 where enumerated = boundedEnumerated
instance Enumerable Word8 where enumerated = boundedEnumerated
{- | 

>>> (maxBound::Word16) - (minBound::Word16) 
65535

-}
instance Enumerable Int16 where enumerated = boundedEnumerated
instance Enumerable Word16 where enumerated = boundedEnumerated
{- | there are only a million (1,114,112) characters.  

>>> ord minBound
0

>>> ord maxBound
1114111

>>> length [chr 0..]
1114112

-}
instance Enumerable Char where enumerated = boundedEnumerated

-- sum types 
instance (Enumerable a) => Enumerable (Maybe a)
instance (Enumerable a, Enumerable b) => Enumerable (Either a b)

-- product types 
instance (Enumerable a, Enumerable b) => Enumerable (a, b)
instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (a, b, c)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d) => Enumerable (a, b, c, d)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e) => Enumerable (a, b, c, d, e)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f) => Enumerable (a, b, c, d, e, f)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f, Enumerable g) => Enumerable (a, b, c, d, e, f, g)

{- | for non-'Generic' Bounded Enums:

@
instance Enumerable ... where
 'enumerated' = boundedEnumerated
@

-}
boundedEnumerated :: (Bounded a, Enum a) => [a]
boundedEnumerated = enumFromTo minBound maxBound

-- | may be unbounded.
enumEnumerated :: (Enum a) => [a]
enumEnumerated = enumFrom (toEnum 0)

-- -- | reify a total function
-- reifyFunction :: (Enumerable a) => (a -> b) -> Map a b
-- -- reifyFunction :: (Enumerable a) => (a -> b) -> Map a b
-- reifyFunction = reifyFunctionM . return
-- {-# INLINABLE reifyFunction #-}

-- reifyFunctionAt :: [a] -> (a -> b) -> Map a b
-- -- reifyFunctionAt :: [a] -> (a -> b) -> Map a b
-- reifyFunctionAt domain = reifyFunctionAtM domain . return
-- {-# INLINABLE reifyFunctionAt #-}

-- -- | reify a partial function into a map (which is implicitly partial, where @Map.lookup@ is like @($)@.
-- reifyFunctionM :: (Enumerable a) => (forall m. MonadThrow m => a -> m b) -> Map a b
-- -- reifyFunctionM :: (MonadThrow m, Enumerable a) => (a -> m b) -> Map a b
-- reifyFunctionM= reifyFunctionAtM enumerated
-- {-# INLINABLE reifyFunctionM #-}

{- | reify a partial function at any domain. 
use when your domain isn't 'Enumerable'. 
the most general function in this module.

>>> :set +m
>>> :{
let uppercasePartial :: Char -> Possibly Char 
    uppercasePartial c = case c of
     'a' -> return 'A'
     'b' -> return 'B'
     'z' -> return 'Z'
     _   -> failed "uppercasePartial"
:}

>>> reifyFunctionAtM ['a'..'c'] uppercasePartial
[('a','A'),('b','B')] 

@ 

-}
reifyFunctionAtM :: [a] -> (a -> Possibly b) -> [(a,b)]
-- reifyFunctionAtM :: [a] -> (a -> (forall m. MonadThrow m => m b)) -> Map a b
-- reifyFunctionAtM :: [a] -> (forall m. MonadThrow m => a -> m b) -> Map a b
-- reifyFunctionAtM :: (MonadThrow m) => [a] -> (a -> m b) -> Map a b
reifyFunctionAtM domain f 
 = concatMap (bitraverse pure id)
 . fmap (id &&& f)
 $ domain
 where
 bitraverse f g (x,y) = (,) <$> f x <*> g y  -- avoid bifunctors dependency

reifyPredicateAtM :: [a] -> (a -> Bool) -> [a]
reifyPredicateAtM domain p = map fst (reifyFunctionAtM domain f)
 where
 f x = if p x then return x else throwM (ErrorCall "False")

-- MonadThrow Maybe	 
-- (e ~ SomeException) => MonadThrow (Either e)
-- MonadThrow []	 

-- reifyFunctionMaybe 
-- reifyFunctionEither 
-- reifyFunctionList

-- forces function to be strict 
-- reifyFunctionSpoon 


-- -- | reify a binary function
-- reifyFunction2AtM :: (MonadThrow m) => [a] -> [b] -> (a -> b -> m c) -> Map a (Map b c)
-- reifyFunction2AtM as bs f = reifyFunctionAt as (reifyFunctionAtM bs . f)

-- reifyFunction2 :: (Enumerable a, Enumerable b) -> (a -> b -> c) -> Map a (Map b c)
-- reifyFunction2 f = reifyFunction2AtM enumerated enumerated (\x y -> pure (f x y))
-- {-# INLINABLE reifyFunction2 #-}

-- module Enumerable.Function.Instances where

-- -- | brute-force function extensionality 
-- instance (Enumerable a, Eq b) => Eq (a -> b) where
--   f == g = all ((==) <$> f <*> g) enumerate
--   f /= g = any ((/=) <$> f <*> g) enumerate

-- -- | finite but too big. @2^64@ is over a billion billion (@1,000,000,000,000@). e.g. 'Enumerate.reifyFunction' on a function of type @(:: Int -> Bool)@ won't realistically terminate TODO is this true? 
-- module Enumerable.Numbers.Instances where

-- instance Enumerable.Large where

{- | (for documentation) 

needs @\{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass \#\-\}@

demonstrates: empty type, unit type, product type, sum type, type variable.

-}
data DemoEnumerable a
 = DemoEnumerable0 Void
 | DemoEnumerable1
 | DemoEnumerable2 Bool (Maybe Bool) 
 | DemoEnumerable3 a
 deriving (Show,Generic,Enumerable) 

{- | (for documentation) 

@demoEnumerated = enumerated@

>>> demoEnumerated
[DemoEnumerable1,DemoEnumerable2 False Nothing,DemoEnumerable2 False (Just False),DemoEnumerable2 False (Just True),DemoEnumerable2 True Nothing,DemoEnumerable2 True (Just False),DemoEnumerable2 True (Just True),DemoEnumerable3 False,DemoEnumerable3 True]

-}
demoEnumerated :: [DemoEnumerable Bool] 
demoEnumerated = enumerated

