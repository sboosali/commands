{-# LANGUAGE KindSignatures, RankNTypes #-}
module Data.HFoldable where 
import Data.HTypes


{-| higher-order Foldable. 

-}
class HFoldable (h :: (* -> *) -> (* -> *)) where
  hfoldMap :: Monoid m => (forall b. f b -> m) -> h f a -> m

instance HFoldable HIdentity where 
 hfoldMap u (HIdentity f) = u f

instance HFoldable (HConst f) where 
 hfoldMap _ _ = mempty  -- TODO 


-- ================================================================ --
