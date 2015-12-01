{-# LANGUAGE KindSignatures, StandaloneDeriving, RankNTypes, TypeOperators, TypeFamilies #-}

{- |

-}
module Data.HFix where
import Data.HFunctor
import Data.HFunctor.Recursion


{-| 

-}
newtype HFix (h :: (* -> *) -> (* -> *)) a = HFix { unHFix :: h (HFix h) a }

type instance HBase (HFix h) = h 

instance (HFunctor h) => HRecursive (HFix h) where hproject = unHFix

instance (HFunctor h) => HCoRecursive (HFix h) where hinject = HFix

