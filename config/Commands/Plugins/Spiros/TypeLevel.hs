{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds, DataKinds, KindSignatures #-}

module Commands.Plugins.Spiros.TypeLevel where
import Data.Kind (Constraint)


{-| the type has satisfies constraint in the (type-level) list.

-}
type family HasEach 
 (cs :: [* -> Constraint]) 
 (a :: *) 
 :: Constraint where
 HasEach '[]       a = ()
 HasEach (c ': cs) a = (c a, HasEach cs a)

{-| each type in the (type-level) list satisfies each constraint in the (type-level) list.

-}
type family EachHasEach 
 (cs :: [* -> Constraint]) 
 (as :: [*]) 
 :: Constraint where
 EachHasEach cs '[]       = ()
 EachHasEach cs (a ': as) = (HasEach cs a, EachHasEach cs as)

