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
import Data.HTraversable

-- import Data.Functor.Product
-- import Data.Functor.Sum
import Control.Category ((>>>), (<<<)) 
import Control.Monad((<=<), (>=>))     -- 


{-| 

-}
newtype HFix (h :: (* -> *) -> (* -> *)) a = HFix { unHFix :: h (HFix h) a }


-- ================================================================ --

{-| "higher-order algebra" 

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

{-| "higher-order algebra, monadic".  

-}
type HAlgebraM m h f = h f :~> (m :. f)

{-| 

-}
hcataM :: (Monad m, HTraversable h) => HAlgebraM m h f -> (HFix h :~> (m :. f))
hcataM algebra = (unHFix >>> return) >=> htraverse (hcataM algebra) >=> algebra 


{-| 

-}
hparaM :: (Monad m, HTraversable h) => (h (HFix h :*: f) :~> (m :. f)) -> (HFix h :~> (m :. f))
hparaM algebra = (unHFix >>> return) >=> htraverse (return <&&&> hparaM algebra) >=> algebra 


-- ================================================================ --

{-| "higher-order coalgebra". 

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

{-| "higher-order coalgebra, monadic".  

-}
type HCoAlgebraM m h f = f :~> (m :. h f)


{-| 

-}
hanaM :: (Monad m, HTraversable h) => (f :~> (m :. h f)) -> (f :~> (m :. HFix h))
hanaM coalgebra = (return <<< HFix) <=< htraverse (hanaM coalgebra) <=< coalgebra


{-| 

-}
hapoM :: (Monad m, HTraversable h) => (f :~> (m :. h (HFix h :+: f))) -> (f :~> (m :. HFix h))
hapoM coalgebra = (return <<< HFix) <=< htraverse (return <|||> hapoM coalgebra) <=< coalgebra


-- ================================================================ --

