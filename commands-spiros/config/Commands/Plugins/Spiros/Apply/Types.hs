{-# LANGUAGE LambdaCase, TypeOperators, GADTs, ConstraintKinds, DataKinds #-}
module Commands.Plugins.Spiros.Apply.Types where 
import Commands.Plugins.Spiros.TypeLevel

import Control.DeepSeq (NFData(..)) 

import Language.Haskell.TH.Syntax (Name)


-- | "freeze" function application, up to arity @4@.
-- the arguments are existentially quantified, but can be constrained.
data Apply constraints r where
 A0 :: ()
    => Name ->                      r                       -> Apply constraints r    -- lol
 A1 :: (EachHasEach constraints '[a])                                           
    => Name -> (a ->                r) -> a                 -> Apply constraints r
 A2 :: (EachHasEach constraints '[a,b])
    => Name -> (a -> b ->           r) -> a -> b            -> Apply constraints r
 A3 :: (EachHasEach constraints '[a,b,c])
    => Name -> (a -> b -> c ->      r) -> a -> b -> c       -> Apply constraints r
 A4 :: (EachHasEach constraints '[a,b,c,d]) 
    => Name -> (a -> b -> c -> d -> r) -> a -> b -> c -> d  -> Apply constraints r

instance Functor (Apply constraints) where fmap = mapApply

instance NFData (Apply constraints r) where -- TODO should be: instance (NFData \in constraints) => NFData (Apply constraints r)
 rnf = \case 
  A0 n x         -> n `seq` x `seq` () 
  A1 n f a       -> n `seq` f `seq` a `seq` () 
  A2 n f a b     -> n `seq` f `seq` a `seq` b `seq` () 
  A3 n f a b c   -> n `seq` f `seq` a `seq` b `seq` c `seq` () 
  A4 n f a b c d -> n `seq` f `seq` a `seq` b `seq` c `seq` d `seq` () 

mapApply :: (a -> b) -> (Apply constraints a -> Apply constraints b)
mapApply g = \case
 A0 n x         -> A0 n (g x)
 A1 n f a       -> A1 n (g.f)     a
 A2 n f a b     -> A2 n (g.!!f)   a b
 A3 n f a b c   -> A3 n (g.!!!f)  a b c
 A4 n f a b c d -> A4 n (g.!!!!f) a b c d

runApply :: Apply constraints r -> r
runApply = \case
 A0 _ f         -> f
 A1 _ f a       -> f a
 A2 _ f a b     -> f a b
 A3 _ f a b c   -> f a b c
 A4 _ f a b c d -> f a b c d

getApplyName :: Apply constraints r -> Name 
getApplyName = \case
 A0 name _         -> name 
 A1 name _ _       -> name 
 A2 name _ _ _     -> name 
 A3 name _ _ _ _   -> name 
 A4 name _ _ _ _ _ -> name 

(.!!)   :: (t1 -> t2) -> (a -> b ->           t1) -> (a -> b ->           t2)
(.!!!)  :: (t1 -> t2) -> (a -> b -> c ->      t1) -> (a -> b -> c ->      t2)
(.!!!!) :: (t1 -> t2) -> (a -> b -> c -> d -> t1) -> (a -> b -> c -> d -> t2)

(.!!)   f g = \a b     -> f (g a b)
(.!!!)  f g = \a b c   -> f (g a b c)
(.!!!!) f g = \a b c d -> f (g a b c d)

