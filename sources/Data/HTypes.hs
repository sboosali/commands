{-# LANGUAGE KindSignatures, RankNTypes, TypeOperators, LambdaCase #-}
module Data.HTypes where 

import Data.Functor.Product
import Data.Functor.Sum
-- import Control.Applicative (Alternative(..)) 


{-| a higher-order identity-functor. 

@HIdentity f ~ f@ 

@
'Identity'  ::  *       ->  * 
'HIdentity' :: (* -> *) -> (* -> *)
@

-}
newtype HIdentity f a = HIdentity { getHIdentity :: f a }

instance (Functor f) => Functor (HIdentity f) where 
 fmap f (HIdentity x) = HIdentity (fmap f x)


-- ================================================================ --

{-| a higher-order constant-functor. 

trivially "lifts" a "lower-order functor" into a "higher-order functor".

@HConst f g ~ f@ 

@
'Const'  ::  *       ->  (* -> *) 
'HConst' :: (* -> *) -> ((* -> *) -> (* -> *)) 
@
 

-}
newtype HConst f (g :: * -> *) a = HConst { getHConst :: f a }

instance (Functor f) => Functor (HConst f g) where 
 fmap f (HConst x) = HConst (fmap f x)

-- -- | @Alternative@ is a lifted @Monoid@. 
-- instance (Alternative f) => Applicative (HConst f g) where
--  pure _ = HConst empty  
--  (<*>) (HConst f) (HConst g) = HConst (f <|> g)

-- instance (Monoid (f x)) => Applicative (HConst f) where -- illegal context 
--  pure = HConst mempty  
--  (<*>) (HConst f) (HConst g) = HConst (f <> g)

-- class HMonoid f where           -- Alternative 
--  hmempty :: f a 
--  hmappend :: f a -> f a -> f a 

-- instance (HMonoid f) => Applicative (HConst f) where
--  pure = HConst hmempty 
--  (<*>) (HConst f) (HConst g) = HConst (f `hmappend` g)

-- ================================================================ --

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


