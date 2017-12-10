{-# LANGUAGE GADTs #-} 
module Data.Some where 

-- import Data.Hashable 

import Data.Type.Equality
-- import Data.Ord


data Some f where 
 Some :: f x -> Some f 

-- 'TestEquality' is a more informative @Eq1@ 
instance (TestEquality f) => Eq (Some f) where -- TODO or Eq1? or TestCoercion?  
 Some x == Some y = maybe2bool (testEquality x y)

-- -- https://hackage.haskell.org/package/transformers-0.5.0.0/docs/Data-Functor-Classes.html#t:Eq1
-- instance (Eq1 f) => Eq (Some f) where
--  Some x == Some y = liftEq ? x y 

-- instance (Hashable (f a)) => Hashable (Some f) where -- NOTE uses UndecidableInstances and AllowAmbiguousTypes
--   hashWithSalt n (Some f) = hashWithSalt n f 

-- https://hackage.haskell.org/package/hashable-extras-0.2.2

-- instance (Hashable1 f) => Hashable (Some f) where -- TODO, StableName has a phantom type, which doesn't need Hashable 
--   hashWithSalt n (Some f) = hashWithSalt1 n f 

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True) 
{-# INLINEABLE maybe2bool #-}

