{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Data.Enumerate where
import Generics.Deriving
import Data.Proxy
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Arrow ((&&&))
import Data.Foldable ((<$>))
-- http://www.haskell.org/haskellwiki/GHC.Generics
-- https://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-Generics.html

reifyFunction :: (Enumerable a) => (a -> b) -> Map a b
reifyFunction f = Map.fromList ((id &&& f) <$> (enumerated (Proxy :: Proxy a))

{- | 

must satisfy "completeness":

    ∀(a::*) (x::a). x `'elem'` ('enumerated' (::a))

must coincide with 'Enum' on types whose constructors all have zero fields (i.e. 'U1')

    ∀(a::*). (Enum a, Bounded a) => ('enumerated' (::a)) == 'constructors'

(notation in pseudo-logic).

-}
class Enumerate a where
 enumerated :: proxy a -> [a]

 default enumerated :: (Generic a, GEnumerate (Rep a) a) => proxy a -> [a]
 enumerated = gEnumerated (Proxy :: Proxy (Rep a x))

-- |
class GEnumerate f a where
 gEnumerated :: proxy (f a) -> [a]

-- |
instance GEnumerate U1 where
 gEnumerated _ = []
-- |
instance (Enumerate a) => GEnumerate (K1 i a) where
 gEnumerated (K1 _) = enumerated (Proxy :: Proxy a)
-- |
instance (GEnumerate f, GEnumerate g) => GEnumerate (f :*: g) where
 gEnumerated (_ :*: _) = 

-- |
instance GEnumerate V1 where
 gEnumerated V1 =
-- |
instance (GEnumerate f, GEnumerate g) => GEnumerate (f :+: g) where
 gEnumerated (L1 x) =
 gEnumerated (R1 x) =
-- |
instance (GEnumerate f) => GEnumerate (M1 S selector f) where
 gEnumerated (M1 x) =
-- |
instance (GEnumerate f) => GEnumerate (M1 C constructor f) where
 gEnumerated (M1 x) =
-- |
instance (GEnumerate f) => GEnumerate (M1 D datatype f) where
 gEnumerated (M1 x) =


{-

`enumerated` is a function
manual instances don't need a `Generic` instance

`default enumerated` is a generic function
automatic instances use the `Generic` instance


V1 instance means: datatype has 0 constructors
(:+:) instance means: datatype has ≥2 constructors

U1 instance means: constructor has 0 fields
K1 instance means: constructor has 1 field
(:*:) instance means: constructor has ≥2 field

`f` and `g` are representations, one of: V1 (:+:) U1 K1 (:*:) M1
`a` is the instance type itself (for K1)


class Generic a where
  type Rep a :: * -> *
  from :: a -> (Rep a) x
  to   :: (Rep a) x -> a


M1 D D_Type V1
M1 D D_Type (M1 C C_Constructor _)
M1 D D_Type (M1 C C_Constructor (_ :+: _))

M1 C C_Constructor (M1 S _) 

M1 S NoSelector U1
M1 S NoSelector (K1 _ Type)
M1 S NoSelector (_ :*: _)

K1 P
K1 R


cannot derive Generic instances for:
datatypes with context
existentially-quantified datatypes
GADTs


data    V1        p                       -- lifted Empty
data    U1        p = U1                  -- lifted ()
data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted Either
data    (:*:) f g p = (f p) :*: (g p)     -- lifted (,) 
newtype K1    i c p = K1 { unK1 :: c }    -- a container for a 'c'
newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper

-}








{- ghci

import Generics.Deriving
:set -XDeriveGeneric
:set -XDefaultSignatures

data Empty  deriving Generic
:kind! Rep Empty
M1 D D1Empty V1

newtype Natural = Natural Integer deriving (Generic, Show)
:kind! Rep Natural
M1 D D1Natural (M1 C C1_0Natural (M1 S NoSelector (K1 R Integer)))

data Bool = True | False  deriving Generic
:kind! Rep Bool
M1 D D1Bool (M1 C C1_0Bool U1 :+: M1 C C1_1Bool U1)

data T = C T T T  deriving Generic
:kind! Rep T
M1 D D1T (M1 C C1_0T (
     M1 S NoSelector (K1 R T)
 :*: M1 S NoSelector (K1 R T)
 :*: M1 S NoSelector (K1 R T)
))


data X = A | B | C  deriving (Generic, Show)

from A
M1 {unM1 = L1 (
 M1 {unM1 = U1})}

from B
M1 {unM1 = R1 (L1 (
 M1 {unM1 = U1})}

from C
M1 {unM1 = R1 (R1 (
 M1 {unM1 = U1})}

to (M1 {unM1 = R1 (R1 (M1 {unM1 = U1}))}) :: X
C

to (M1 {unM1 = L1 (L1 (M1 {unM1 = U1}))}) :: X
-- type error
-- "Couldn't match type ‘M1 C C1_0X U1’ with ‘M1 i0 c0 U1 :+: g0’"


from $ Natural 666
M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = 666}}}}

to (M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 666}}}) :: Natural
Natural 666


-}
