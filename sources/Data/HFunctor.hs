{-# LANGUAGE KindSignatures, RankNTypes, TypeOperators #-}
module Data.HFunctor where  


{-| 

-}
type f :~> g = forall x. f x -> g x


{-| higher-order Functor. 

-}
class HFunctor (h :: (* -> *) -> * -> *) where
  hfmap :: (f :~> g) -> (h f :~> h g)


{-| a higher-order constant-functor. 

trivially "lifts" a "lower-order functor" into a "higher-order functor".

@
'Const'  ::  *       ->  (* -> *) 
'HConst' :: (* -> *) -> ((* -> *) -> (* -> *)) 
@


-}
newtype HConst f (g :: * -> *) a = HConst { unHConst :: f a }


