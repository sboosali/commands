{-# LANGUAGE KindSignatures, StandaloneDeriving, RankNTypes, TypeOperators #-}

{-| higher-order type-constructor fixed-point. 

see <http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html>  

-}
module Data.HFix
 ( module Data.HFix
 , module Data.HFunctor 
 , module Data.HFoldable 
 ) where  
import Data.HFunctor 
import Data.HFoldable

-- import Data.Functor.Product
-- import Data.Functor.Sum
-- TODO import Control.Arrow ((>>>), (<<<)) 


{-| 

-}
newtype HFix (h :: (* -> *) -> (* -> *)) a = HFix { unHFix :: h (HFix h) a }


-- ================================================================ --


{-| 

-}
type HAlgebra h f = h f :~> f 


{-| 

e.g. expanding the type aliases: 

@
::   HAlgebra ExprF (K String)                     -> (          Expr         :~>  K String   )
:: (          ExprF (K String)   :~>  K String   ) -> (          HFix ExprF   :~>  K String   )
:: (forall x. ExprF (K String) x  -> (K String) x) -> (forall x. HFix ExprF x  -> (K String) x)
@


-}
hcata :: (HFunctor h) => HAlgebra h f -> (HFix h :~> f)
hcata algebra = algebra . hfmap (hcata algebra) . unHFix


{-| 

-}
hpara :: (HFunctor h) => (h (HFix h :*: f) :~> f) -> (HFix h :~> f)
hpara algebra = algebra . hfmap (id .&&&. hpara algebra) . unHFix 



-- ================================================================ --


{-| 

-}
type HCoAlgebra h f = f :~> h f 


{-| 

-}
hana :: (HFunctor h) => (f :~> h f) -> (f :~> HFix h)
hana coalgebra = HFix . hfmap (hana coalgebra) . coalgebra


{-| 

-}
hapo :: (HFunctor h) => (f :~> h (HFix h :+: f)) -> (f :~> HFix h)
hapo coalgebra = HFix . hfmap (id .|||. hapo coalgebra) . coalgebra


-- ================================================================ --


{-| 

-}
type HCoAlgebraM m h f = f :~> (m :. h f)


-- {-| 

-- -}
-- hanaM :: (HFunctor h) => (f :~> (m :. h f)) -> (f :~> (m :. HFix h))
-- hanaM coalgebra = HFix . hfmap (hanaM coalgebra) . coalgebra


-- {-| 

-- -}
-- hapoM :: (HFunctor h) => (f :~> m (h :. (HFix h :+: f)) -> (f :~> (m :. HFix h))
-- hapoM coalgebra = HFix . hfmap (id .|||. hapoM coalgebra) . coalgebra


