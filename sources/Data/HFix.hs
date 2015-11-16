{-# LANGUAGE KindSignatures, StandaloneDeriving, RankNTypes, TypeOperators #-}

{-| higher-order type-constructor fixed-point. 

see <http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html>  

-}
module Data.HFix
 ( module Data.HFix
 , module Data.HFunctor 
 ) where  
import Data.HFunctor 


{-| 

-}
newtype HFix h a = HFix { unHFix :: h (HFix h) a }


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
hcata :: HFunctor h => HAlgebra h f -> (HFix h :~> f)
hcata algebra = algebra . hfmap (hcata algebra) . unHFix



-- ================================================================ --


{-| 

-}
type HCoAlgebra h f = f :~> h f 


{-| 

-}
hana :: HFunctor h => HCoAlgebra h f -> (f :~> HFix h)
hana coalgebra = HFix . hfmap (hana coalgebra) . coalgebra


