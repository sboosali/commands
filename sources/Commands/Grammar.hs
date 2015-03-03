{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase, PackageImports, RankNTypes, ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, TypeOperators                #-}
-- |
module Commands.Grammar where
import           Commands.Etc
import           Commands.Grammar.Types
import           Commands.Munging
import           Control.Alternative.Free.Tree

import           Control.Applicative
import           Control.Monad.Trans.State.Strict
-- import           Data.Foldable                     (traverse_)
import           Data.List                        (intercalate)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Language.Haskell.TH.Syntax       (Name)
-- import Control.Monad.State hiding  (lift)
-- import           Control.Monad                     (when)
import           Data.Functor.Constant
import           "transformers-compat" Data.Functor.Sum


infix  2 <=>
-- infixl 3 <|>
-- infixl 4 <$>
infixl 4 #
-- infixl 4 <*>
infixl 9 &

(<=>) :: Name -> RHS a -> Rule a
name <=> rs = Rule l rs
 where Just l = fromName name

-- (&) :: (Grammatical a) => RHSs (a -> b) -> a -> RHSs b
-- f & x = f <*> toR x
(&) :: Applicative f => f (a -> b) -> f a -> f b
f & x = f <*> x

-- (#) :: (Grammatical a) => (a -> b) -> a -> RHSs b
-- f # x = f <$> toR x
(#) :: Functor f => (a -> b) -> f a -> f b
f # x = f <$> x

-- class Grammatical a where
--  type R a :: *
--  toR :: a -> RHSs (R a)

-- instance Grammatical (RHSs b)    where  type R (RHSs b)    = b;  toR = id
-- instance Grammatical String      where  type R String      = b;  toR = lift . Terminal
-- -- instance Grammatical (Rule b) where  type R (Rule b) = b;  toR = lift
-- instance Grammatical (Rule b) where
--  type R (Rule b) = b
--  toR (Terminal s)      = toR s
--  toR (NonTerminal _ r) = r

project :: Rule a -> RHS a
project = lift . InR

terminal :: String -> RHS a
terminal = lift . InL . Constant . Word

terminals :: [String] -> RHS String
terminals = foldr (<|>) empty . map str

con :: (Show a) => a -> RHS a
con c = c <$ (terminal . intercalate " " . unCamelCase . show) c

int :: Int -> RHS Int
int = con

str :: String -> RHS String
str s = s <$ terminal s

twig :: (Enum a, Show a) => RHS a
twig = foldr (<|>) empty . map con $ constructors

reifyRule :: Rule x -> Map LHS (Some RHS)
reifyRule grammar = execState (reifyRule_ grammar) Map.empty

reifyRule_ :: Rule x -> State (Map LHS (Some RHS)) ()
reifyRule_ = undefined
