{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures, TypeOperators, LambdaCase #-}
module Data.HFunctor where 
import Data.HTypes


{-| higher-order Functor. 

-}
class HFunctor (h :: (* -> *) -> (* -> *)) where
 hfmap :: (f :~> g) -> (h f :~> h g)

instance HFunctor HIdentity where 
 hfmap u (HIdentity f) = HIdentity (u f)

instance HFunctor (HConst f) where 
 hfmap _ (HConst f) = (HConst f) 

