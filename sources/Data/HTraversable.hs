{-# LANGUAGE KindSignatures, RankNTypes, TypeOperators #-}
module Data.HTraversable where 
import Data.HFoldable 
import Data.HFunctor 


{-| higher-order Foldable. 

-}
class (HFunctor h, HFoldable h) => HTraversable (h :: (* -> *) -> (* -> *)) where
  htraverse :: Applicative m => (f :~> (m :. g)) -> (h f :~> (m :. h g))


-- ================================================================ --

