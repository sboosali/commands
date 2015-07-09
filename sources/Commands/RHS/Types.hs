{-# LANGUAGE DeriveFunctor, FlexibleContexts, GADTs, LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms, PatternSynonyms, RankNTypes   #-}
{-# LANGUAGE StandaloneDeriving, TypeOperators, ScopedTypeVariables #-}
module Commands.RHS.Types where

-- import           Control.Lens
import           Data.List.NonEmpty(NonEmpty (..))
import qualified Data.List.NonEmpty    as NonEmpty

import           Control.Applicative
import           Data.Monoid
import           Data.Foldable                 (asum)


data RHS n t f a where

 -- Applicative instance
 Pure        :: a                                     -> RHS n t f a
 Apply       :: (RHS n t f (x -> a)) -> (f x)         -> RHS n t f a
 (:<*>)      :: (RHS n t f (x -> a)) -> (RHS n t f x) -> RHS n t f a

 -- Alternative/Monoid instance
 Alter       :: ![RHS n t f a] -> RHS n t f a

 -- (Coyoneda'd?) methods
 Opt         :: !(Maybe x    -> a) -> RHS n t f x -> RHS n t f a
 Many        :: !([x]        -> a) -> RHS n t f x -> RHS n t f a
 Some        :: !(NonEmpty x -> a) -> RHS n t f x -> RHS n t f a

 -- (Coyoneda'd?) grammar-specific stuff
 Terminal    :: !(t -> a) -> !t -> RHS n t f a
 NonTerminal :: !(n t f a)      -> RHS n t f a

pattern Empty = Alter []

-- TODO expects constraint: 
deriving instance (Functor (n t f)) => (Functor (RHS n t f))


instance (Functor f, Functor (n t f)) => Monoid (RHS n t f a) where
 mempty = Empty
 mappend = (<|>)

instance (Functor f, Functor (n t f)) => Applicative (RHS n t f) where
 pure = Pure

 Pure xa <*> tx                = fmap xa tx                       -- Functor
 -- Pure {id} <*> x            = fmap {id} x     = x              -- Identity
 -- Pure f <*> Pure x          = fmap f (Pure x) = Pure (f x)     -- Homomorphism
 txa     <*> Pure x            = fmap ($ x) txa                   -- Interchange

 Empty   <*> _                 = Empty                          -- left-Annihilation (?)
 _       <*> Empty             = Empty                          -- right-Annihilation
 txa     <*> Alter txs         = txa :<*> Alter txs               -- NO left-Distributivity

 txa     <*> (tyx `Apply` fy)  = ((.) <$> txa <*> tyx) `Apply` fy -- Composition
 txa     <*> (tyx :<*>    ty)  = ((.) <$> txa <*> tyx) :<*>    ty -- Composition

 txa     <*> (Opt  ysa ty)     = txa :<*> Opt  ysa ty -- TODO correct?
 txa     <*> (Many ysa ty)     = txa :<*> Many ysa ty
 txa     <*> (Some ysa ty)     = txa :<*> Some ysa ty

 txa     <*> (Terminal    i t) = txa :<*> Terminal    i t -- TODO correct?
 txa     <*> (NonTerminal na)  = txa :<*> NonTerminal na

instance (Functor f, Functor (n t f)) => Alternative (RHS n t f) where
 empty = Empty

 Empty <|> y = y                            -- Left-Identity
 x <|> Empty = x                            -- Right-Identity
 x <|> y = Alter (toRHSList x <> toRHSList y) -- Associativity
 {-# INLINE (<|>) #-}

 many = Many id
 {-# INLINE many #-}
 some = fmap NonEmpty.toList . Some id
 {-# INLINE some #-}

-- | a reified 'optional'.
optionalA :: RHS n t f a -> RHS n t f (Maybe a)
optionalA = Opt id
{-# INLINE optionalA #-}

optionA :: a -> RHS n t f a -> RHS n t f a
optionA x = Opt (maybe x id)
{-# INLINE optionA #-}

toRHSList :: RHS n t f a -> [RHS n t f a]
toRHSList (Alter xs) = xs
toRHSList x = [x]
{-# INLINE toRHSList #-}

-- |
liftRHS :: f a -> RHS n t f a
liftRHS f = Pure id `Apply` f
{-# INLINE liftRHS #-}

runRHS
 :: forall n t f g a. (Alternative g)
 => (forall x. n t f x -> g x)
 -> (forall x. t       -> g x)
 -> (forall x. f x     -> g x)
 -> RHS n t f a
 -> g a
runRHS fromNonTerminal fromTerminal u = \case
 Terminal    i t -> i <$> fromTerminal    t
 NonTerminal n   ->       fromNonTerminal n

 Opt  i x  -> i                       <$> optional (go x)
 Many i x  -> i                       <$> many     (go x)
 Some i x  -> (i . NonEmpty.fromList) <$> some     (go x)

 Pure a      -> pure a
 f `Apply` x -> go f <*> u x
 f :<*>    g -> go f <*> go g
 -- RHS [] -> empty
 -- RHS [f,g]  -> go f <|> go g
 Alter fs  -> asum (go `map` fs)
 where
 go :: forall x. RHS n t f x -> g x
 go = runRHS fromNonTerminal fromTerminal u

