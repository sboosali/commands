{-# LANGUAGE KindSignatures, RankNTypes #-}
module Data.HFoldable where 


{-| higher-order Foldable. 

-}
class HFoldable (h :: (* -> *) -> (* -> *)) where
  hfoldMap :: Monoid m => (forall b. f b -> m) -> h f a -> m


-- ================================================================ --
