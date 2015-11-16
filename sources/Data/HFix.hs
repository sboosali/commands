{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, StandaloneDeriving, RankNTypes, TypeOperators #-}


{-| higher-order type-constructor fixed-point. 

see <http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html>  

-}
module Data.HFix where  


newtype HFix h a = HFix { unHFix :: h (HFix h) a }

type f :~> g = forall x. f x -> g x

class HFunctor (h :: (* -> *) -> * -> *) where
  hfmap :: (f :~> g) -> (h f :~> h g)

type HAlgebra h f = h f :~> f 

{-| 

e.g. expanding the type aliases: 

@
::   HAlgebra ExprF (K String)                     -> (          Expr         :~>  K String   )
:: (          ExprF (K String)   :~>  K String   ) -> (          HFix ExprF   :~>  K String   )
:: (forall x. ExprF (K String) x  -> (K String) x) -> (forall x. HFix ExprF x  -> (K String) x)
@

-}
hcata :: HFunctor h => HAlgebra h f -> (HFix h :~> f)
hcata alg = alg . hfmap (hcata alg) . unHFix

{-| a higher-order constant-functor. 

trivially "lifts" a "lower-order functor" into a "higher-order functor".

@
'Const'  ::  *       ->  (* -> *) 
'HConst' :: (* -> *) -> ((* -> *) -> (* -> *)) 
@


-}
newtype HConst f (g :: * -> *) a = HConst { unHConst :: f a }

