{-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, LambdaCase #-}
module Data.HFunctor where 

import Data.Functor.Product
import Data.Functor.Sum


{-| 

-}
type f :~> g = forall x. f x -> g x


{-| 

-}
type (f :. g) a = f (g a)


type f :+: g = Sum f g 

{-| eliminate a sum.  

-}
(.|||.) :: (f1 :~> g) -> (f2 :~> g) -> ((f1 :+: f2) :~> g)
(.|||.) u1 u2 = \case 
 InL f -> u1 f 
 InR f -> u2 f 

type f :*: g = Product f g 

getFirst :: Product f g a -> f a 
getFirst (Pair f _) = f 

getSecond :: Product f g a -> g a 
getSecond (Pair _ g) = g 

{-| introduce a product. 

-}
(.&&&.) :: (f :~> g1) -> (f :~> g2) -> (f :~> (g1 :*: g2))
(.&&&.) u1 u2 = \f -> Pair (u1 f) (u2 f) 


{-| higher-order Functor. 

-}
class HFunctor (h :: (* -> *) -> (* -> *)) where
  hfmap :: (f :~> g) -> (h f :~> h g)


{-| a higher-order constant-functor. 

trivially "lifts" a "lower-order functor" into a "higher-order functor".

@
'Const'  ::  *       ->  (* -> *) 
'HConst' :: (* -> *) -> ((* -> *) -> (* -> *)) 
@


-}
newtype HConst f (g :: * -> *) a = HConst { unHConst :: f a }


