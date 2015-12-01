{-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, LambdaCase #-}
module Data.HTypes where 

import Data.Functor.Product
import Data.Functor.Sum


{-| a higher-order identity-functor. 

@HIdentity f ~ f@ 

@
'Identity'  ::  *       ->  * 
'HIdentity' :: (* -> *) -> (* -> *)
@

-}
newtype HIdentity f a = HIdentity { getHIdentity :: f a }


{-| a higher-order constant-functor. 

trivially "lifts" a "lower-order functor" into a "higher-order functor".

@HConst f g ~ f@ 

@
'Const'  ::  *       ->  (* -> *) 
'HConst' :: (* -> *) -> ((* -> *) -> (* -> *)) 
@
 

-}
newtype HConst f (g :: * -> *) a = HConst { getHConst :: f a }


{-| natural transformation (i.e. functor transformation).

-}
type f :~> g = forall x. f x -> g x

{-| functor composition.

-}
type (f :. g) a = f (g a)

{-| functor sum.

-}
type f :+: g = Sum f g 

{-| functor product.

-}
type f :*: g = Product f g 

{-| eliminate a sum.  

-}
(.|||.) :: (f1 :~> g) -> (f2 :~> g) -> ((f1 :+: f2) :~> g)
(.|||.) u1 u2 = \case 
 InL f -> u1 f 
 InR f -> u2 f 

{-| eliminate a sum, monadically. 

-}
(<|||>) :: (f1 :~> (m :. g)) -> (f2 :~> (m :. g)) -> ((f1 :+: f2) :~> (m :. g))
(<|||>) u1 u2 = \case 
 InL f -> u1 f 
 InR f -> u2 f 

{-| project from a product.

-}
getFirst :: (f :*: g) :~> f
getFirst (Pair f _) = f 

{-| project from a product.

-}
getSecond :: (f :*: g) :~> g
getSecond (Pair _ g) = g 

{-| introduce a product. 

-}
(.&&&.) :: (f :~> g1) -> (f :~> g2) -> (f :~> (g1 :*: g2))
(.&&&.) u1 u2 = \f -> Pair (u1 f) (u2 f) 

{-| introduce a product, monadically. 

-}
(<&&&>) :: (Applicative m) => (f :~> (m :. g1)) -> (f :~> (m :. g2)) -> (f :~> (m :. (g1 :*: g2)))
(<&&&>) u1 u2 = \f -> Pair <$> (u1 f) <*> (u2 f) 


