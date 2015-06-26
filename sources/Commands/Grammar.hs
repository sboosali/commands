{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, RankNTypes, ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies              #-}
{-# LANGUAGE TypeOperators                                                  #-}
-- |
module Commands.Grammar where
import           Commands.Munging      (unCamelCase)
import           Commands.Symbol.Types

import           Control.Lens

import           Data.Foldable         (asum)
import           Data.List             (intercalate)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set


vocabulary :: [String] -> RHS p r l String String
vocabulary = asum . fmap word

str :: String -> RHS p r l String String
str = word

chr :: (Functor (p String)) => Char -> RHS p r l String Char
chr c = c <$ word [c]

-- | a specialization, @int = 'con'@, because integer literals are 'Num'-constrained polymorphic types.
-- in the context we will be using it, we need a concrete type for type inference.
int :: (Functor (p String)) => Int -> RHS p r l String Int
int = con

con :: (Show a, Functor (p String)) => a -> RHS p r l String a
con = transformedCon (intercalate " " . unCamelCase)

-- | make a 'Terminal' from the @transformed@ 'Show'n constructor, returning the constructor.
transformedCon :: (Show a, Functor (p String)) => (String -> String) -> a -> RHS p r l String a
transformedCon f x = x <$ (word . f . show $ x)

type SomeRules p r l i = Map l (SomeRule p r l i)

-- | finds all the rules in a right-hand side.
--
-- even without the result type, you can still extract their untyped lhs, count them, etc.
--
-- (doesn't terminate on recursive input.)
rhs2rules :: (Ord l) => RHS p r l i x -> SomeRules p r l i
rhs2rules = foldRHS fromSymbol Map.empty (Map.union) (Map.unions) id id id
 where
 fromSymbol = \case
  Terminal _       -> Map.empty
  NonTerminal rule -> Map.singleton (rule ^. ruleLHS) (SomeRule rule)

rhs2words :: (Ord i) => RHS p r l i x -> Set i
rhs2words = foldRHS fromSymbol Set.empty (Set.union) (Set.unions) id id id
 where
 fromSymbol :: (Ord i) => Symbol n i x -> Set i
 fromSymbol = \case
  Terminal t    -> Set.singleton t
  NonTerminal _ -> Set.empty

