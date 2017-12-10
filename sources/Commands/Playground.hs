{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}
module Commands.Playground where 

-- import Data.Vinyl 
-- import Data.Tagged 
-- import GHC.TypeLits (Symbol) 


-- type MyRecord = Rec Field MyTypes

-- type MyTypes = 
--  [ Bool  
--  , Integer 
--  , String 
--  ]


-- data Field field where 
--  Field :: a -> Field 


-- type family TShow :: k -> Symbol  -- should be injective, but not closed  
-- -- type family TRead :: Symbol -> k

-- type instance TShow Bool = "Bool" 
-- type instance TShow Integer = "Integer" 
-- type instance TShow String = "String"


-- type family TMap f as where 
--  TMap f '[] = '[] 
--  TMap f (a ': as) = f a ': TMap f as 

-- data Universe a where
--   NAT  :: Universe Int
--   BOOL :: Universe Bool

-- foo NAT = Just (0::Int)  
-- foo _   = Nothing

-- bar :: Universe a -> Maybe Int
-- bar NAT = Just 0
-- bar _   = Nothing

-- baz :: Universe a -> Maybe a
-- baz NAT = Just 0
-- baz _   = Nothing

