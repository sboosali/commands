{-# LANGUAGE TypeFamilies, KindSignatures, StandaloneDeriving, RankNTypes, TypeOperators, FlexibleContexts #-}

{-| 

-}
module Data.HFunctor.Recursion where 
import Data.HFunctor 
import Data.HFoldable
import Data.HTraversable


-- ================================================================ --

{-| 

-}
type family HBase (h :: * -> *) :: (* -> *) -> (* -> *)


{-| 

e.g. 

@
instance HRecursive ('HFix' h) where hproject = unHFix
-- hproject :: (h0 ~ HFix h) => h0      :~>  ('HBase' h0)       h0
-- hproject ::                  HFix h  :~>  (HBase (HFix h)) (HFix h)
-- hproject ::                  HFix h  :~>  h                (HFix h)
@

-}
class HFunctor (HBase h) => HRecursive (h :: * -> *) where
  hproject :: h :~> (HBase h) h


{-| 

e.g. 

@
instance HCoRecursive ('HFix' h) where hinject = HFix
-- hinject :: (h0 ~ HFix h) => ('HBase' h0)       h0        :~>  h0
-- hinject ::                  (HBase (HFix h)) (HFix h)  :~>  HFix h 
-- hinject ::                  h                (HFix h)  :~>  HFix h
@

-}
class HFunctor (HBase h) => HCoRecursive (h :: * -> *) where 
  hinject :: (HBase h) h :~> h



-- ================================================================ --

{-| 

-}
newtype HFix (h :: (* -> *) -> (* -> *)) a = HFix { unHFix :: h (HFix h) a }

type instance HBase (HFix h) = h 

instance (HFunctor h) => HRecursive (HFix h) where hproject = unHFix

instance (HFunctor h) => HCoRecursive (HFix h) where hinject = HFix

-- type instance HBase (RHS t n h) = RhsF t n h 

-- instance HRecursive (RHS t n h) where hproject = unRHS

-- instance HCoRecursive (RHS t n h) where hinject = RHS


-- ================================================================ --

