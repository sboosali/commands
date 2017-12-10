{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures, TypeOperators, LambdaCase #-}
module Data.HFunctor where 
import Data.HTypes


{-| higher-order Functor. 

instances should satisfy 'Functor', as if:

@
class (forall f. Functor (h f)) => HFunctor h
@

* 'hfmap' changes the "functor": @(hfmap _ :: h f a -> h g a)@
* 'fmap' changes the "output": @(fmap _ :: h f a -> h f b)@ 

-}
class HFunctor (h :: (* -> *) -> (* -> *)) where
 hfmap :: (f :~> g) -> (h f :~> h g)

{-TODO

class (forall f. Functor (h f)) => HFunctor (h :: (* -> *) -> (* -> *))

needs higher-rank constraints

-}

instance HFunctor HIdentity where 
 hfmap u (HIdentity f) = HIdentity (u f)

instance HFunctor (HConst f) where 
 hfmap _ (HConst f) = (HConst f) 
