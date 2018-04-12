{-# LANGUAGE KindSignatures, RankNTypes, TypeOperators #-}
module Data.HTraversable where 
import Data.HTypes
import Data.HFunctor 
-- import Data.HFoldable 

import Prelude

{-| higher-order Traversable. 

-}
class (HFunctor h) => HTraversable (h :: (* -> *) -> (* -> *)) where
-- class (HFunctor h, HFoldable h) => HTraversable (h :: (* -> *) -> (* -> *)) where
 htraverse :: Applicative m => (f :~> (m :. g)) -> (h f :~> (m :. h g))

instance HTraversable HIdentity where 
 htraverse u (HIdentity f) = HIdentity <$> u f

instance HTraversable (HConst f) where 
 htraverse _ (HConst f) = pure (HConst f)


-- ================================================================ --

-- hfmapDefault :: (HTraversable h) => (f :~> g) -> (h f :~> h g)
-- hfmapDefault u = getHIdentity . htraverse ((HIdentity . u) :: (f :~> (HIdentity :. g)))

-- hfoldMapDefault :: (HTraversable h, Monoid m) => (forall b. f b -> m) -> h f a -> m
-- hfoldMapDefault u = getHConst . htraverse (HConst . u)  

