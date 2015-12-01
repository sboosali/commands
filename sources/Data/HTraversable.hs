{-# LANGUAGE KindSignatures, RankNTypes, TypeOperators #-}
module Data.HTraversable where 
import Data.HTypes
import Data.HFunctor 
-- import Data.HFoldable 


{-| higher-order Foldable. 

-}
class (HFunctor h) => HTraversable (h :: (* -> *) -> (* -> *)) where
-- class (HFunctor h, HFoldable h) => HTraversable (h :: (* -> *) -> (* -> *)) where
 htraverse :: Applicative m => (f :~> (m :. g)) -> (h f :~> (m :. h g))

instance HTraversable HIdentity where 
 htraverse u (HIdentity f) = HIdentity <$> u f

instance HTraversable (HConst f) where 
 htraverse _ (HConst f) = pure (HConst f)


-- ================================================================ --

