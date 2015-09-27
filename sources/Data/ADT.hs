{-# LANGUAGE DeriveGeneric, DefaultSignatures, TypeOperators, FlexibleInstances, FlexibleContexts, DeriveAnyClass, TypeFamilies, LambdaCase, EmptyCase  #-}

{- | lowering the lifted 'GHC.Generics' types from @(*->*)@ to @(*)@.  

-}
module Data.ADT where 

import           GHC.Generics
import Data.Void 


{- | 

e.g.  

@
canonical
@

idempotent:

@
canonical . canonical = canonical
@

(TODO nesting maybe different between different data types)

NOTE GHC preserves the order of sums/products, but doesn't guarantee any predictable associativity 

uses: 

* structural equality, not nominal equality (TODO wants consistent)
* treating a custom flag type as a Bool, a custom error type as an Either, et cetera 
* automatic `memotrie` instance (TODO wants balanced)
* automatic isomorphisms 

the definition: @canonical = 'glower' . 'from'@

-} 
class HasCanonical a where
 type Canonical a :: *
 
 canonical :: a -> Canonical a 
 
 default canonical :: (Generic a, GHasCanonical (Rep a), GCanonical (Rep a) ~ Canonical a) => a -> Canonical a 
 canonical = gcanonical . from


instance HasCanonical Void where
 type Canonical Void = Void 
 canonical = \case 


instance HasCanonical ()  where
 type Canonical () = () 
 canonical () = () 


instance (HasCanonical a, HasCanonical b) => HasCanonical (Either a b) where
 type Canonical (Either a b) = Either (Canonical a) (Canonical b)
 canonical (Left  x) = Left  (canonical x)
 canonical (Right y) = Right (canonical y)


instance (HasCanonical a, HasCanonical b) => HasCanonical (a,b) where
 type Canonical (a,b) = (Canonical a, Canonical b)
 canonical (a,b) = (canonical a, canonical b)

instance HasCanonical Bool where 
 type Canonical Bool = Either () () -- TODO


{- | structural equality 

e.g. a flag, isomorphic to @Bool@: 

>>> data Flag = No | Yes deriving (Generic,HasCanonical) 
>>> Yes `structuralEq` True 
True
>>> No `structuralEq` True 
False

e.g. an error or result, isomorphic to @Either String a@: 

>>> data ParseResult a = ParseError String | ParseResult a deriving (Generic,HasCanonical) 
>>> ParseResult True `structuralEq` Right True
True
>>> ParseResult True `structuralEq` Right False
False
>>> ParseError "error" `structuralEq` Left "error" 
True
>>> ParseError "error" `structuralEq` Right False 
False

the "structure" is only the order of the constructors/fields, not field names like record equality.

-}
structuralEq
 :: (HasCanonical a, HasCanonical b, Eq (Canonical a), Eq (Canonical b), Canonical a ~ Canonical b)
 => a -> b -> Bool
structuralEq x y = canonical x == canonical y 


{- | 

-}
class GHasCanonical f where
 type GCanonical f :: *
 gcanonical :: f x -> GCanonical f


-- | the @0@ type
instance GHasCanonical (V1) where
 type GCanonical (V1) = Void 
 gcanonical = \case 


-- | the @1@ type 
instance GHasCanonical (U1) where
 type GCanonical (U1) = () 
 gcanonical (U1) = () 


-- | call 'canonical'
instance (HasCanonical a) => GHasCanonical (K1 i a) where
 type GCanonical (K1 i a) = Canonical a 
 gcanonical (K1 a) = canonical a 


-- | multiply types with @(,)@ 
instance (GHasCanonical (f), GHasCanonical (g)) => GHasCanonical (f :*: g) where
 type GCanonical (f :*: g) = ((GCanonical f), (GCanonical g))
 gcanonical (f :*: g) = (gcanonical f, gcanonical g)


-- | add types with @Either@ 
instance (GHasCanonical (f), GHasCanonical (g)) => GHasCanonical (f :+: g) where
 type GCanonical (f :+: g) = Either (GCanonical f) (GCanonical g) 
 gcanonical (L1 f) = Left  (gcanonical f) 
 gcanonical (R1 g) = Right (gcanonical g) 


-- | (ignore metadata) 
instance (GHasCanonical (f)) => GHasCanonical (M1 i t f) where
 type GCanonical (M1 i t f) =  (GCanonical f)
 gcanonical (M1 f) = gcanonical f


-- TODO data Flag = No | Yes deriving (Generic,HasCanonical) 
data Flag = No | Yes deriving (Generic)
instance HasCanonical Flag where type Canonical Flag = Either () ()

mainADT :: IO ()
mainADT = do 
 let flag1 = No `structuralEq` True          -- False
 let flag2 = Yes `structuralEq` True         -- True

 putStrLn ""
 print "Canonical" 
 print flag1 
 print flag2
