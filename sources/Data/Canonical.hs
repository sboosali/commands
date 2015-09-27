{-# LANGUAGE DeriveGeneric, DefaultSignatures, TypeOperators, FlexibleInstances, FlexibleContexts, DeriveAnyClass, TypeFamilies, LambdaCase, EmptyCase, UndecidableInstances   #-}

{- | lowering the lifted 'GHC.Generics' types from @(*->*)@ to @(*)@.  

-}
module Data.Canonical where 

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
class (Generic a, GHasCanonical (Rep a)) => HasCanonical a where
 type Canonical a :: *
 type Canonical a = GCanonical (Rep a) -- UndecidableInstances

 canonical :: a -> Canonical a 

 default canonical :: (GCanonical (Rep a) ~ Canonical a) => a -> Canonical a 
 canonical = gcanonical . from


{- | (@0@, basecase) 
-}
instance HasCanonical Void where
 type Canonical Void = Void 
 canonical = \case 


{- | (@1@, basecase) 
-}
instance HasCanonical () where
 type Canonical () = () 
 canonical () = () 


{- | (addition, basecase) 
-}
instance (HasCanonical a, HasCanonical b) => HasCanonical (Either a b) where
 type Canonical (Either a b) = Either (Canonical a) (Canonical b)
 canonical (Left  x) = Left  (canonical x)
 canonical (Right y) = Right (canonical y)


{- | (multiplication, basecase) 
-}
instance (HasCanonical a, HasCanonical b) => HasCanonical (a,b) where
 type Canonical (a,b) = (Canonical a, Canonical b)
 canonical (a,b) = (canonical a, canonical b)


-- {- | (recursive, basecase) 
-- -}
-- instance (HasCanonical a) => HasCanonical [a] where
--  type Canonical [a] = 
--  canonical = 

-- instance (HasCanonical a) => HasCanonical [a]  -- TODO incident type, loops the type checker, needs data



{- | (derived) 
-}
instance HasCanonical Bool 

-- {- | (derived) 
-- -}
-- instance HasCanonical Char  -- TODO takes too long 


{- | structural equality 

e.g. a flag, isomorphic to @Bool@: 

>>> data Flag = No | Yes deriving (Generic)
>>> instance 'HasCanonical' Flag
>>> Yes `structuralEq` True 
True
>>> No `structuralEq` True 
False

e.g. an error or result, isomorphic to @Either String a@: 

>>> data ParseResult a = ParseError String | ParseResult a deriving (Generic)
>>> instance ('HasCanonical' a) => 'HasCanonical' (ParseResult a) 
>>> ParseResult True `structuralEq` Right True
True
>>> ParseResult True `structuralEq` Right False
False
>>> ParseError "error" `structuralEq` Left "error" 
True
>>> ParseError "error" `structuralEq` Right False 
False
-- >>> ParseError "error" `structuralEq` "error"
-- type error: the constraint @(Canonical (ParseResult a) ~ Canonical String)@ can't be satisfied 

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
instance HasCanonical Flag
-- instance HasCanonical Flag where type Canonical Flag = (GCanonical (Rep Flag))

-- TODO string is a recursive type: data ParseResult a = ParseError String | ParseResult a deriving (Generic,HasCanonical) 
-- data ParseResult a = ParseError Bool | ParseResult a a a deriving (Generic)
-- instance (HasCanonical a) => HasCanonical (ParseResult a) 
-- type ExampleParseResult = ParseResult Bool

mainCanonical :: IO ()
mainCanonical = do 
 putStrLn ""
 print "Canonical" 

 let flag1 = Yes `structuralEq` True         -- True
 let flag2 = No `structuralEq` True          -- False

 putStrLn "Bool: true/false"
 print flag1 
 print flag2

 -- let result1 = (ParseResult True True True :: ExampleParseResult) `structuralEq` (Right (True,(True,True))    :: Canonical ExampleParseResult)
 -- let result2 = (ParseResult True True True :: ExampleParseResult) `structuralEq` (Right (False,(False,False)) :: Canonical ExampleParseResult)
 -- let result3 = (ParseError False :: ExampleParseResult)           `structuralEq` (Left False                  :: Canonical ExampleParseResult)
 -- let result4 = (ParseError False :: ExampleParseResult)           `structuralEq` (Right (False,(False,False)) :: Canonical ExampleParseResult)

 -- putStrLn "Eithers/Pairs: true/false/true/false"
 -- print result1
 -- print result2 
 -- print result3 
 -- print result4

