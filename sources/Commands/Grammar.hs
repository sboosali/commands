{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving            #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                    #-}
-- |
module Commands.Grammar where
import Commands.Etc
import Commands.Grammar.Types
import Commands.Munging
import Control.Alternative.Free.Johansen

import Control.Applicative
import Data.List                         (intercalate)
import Language.Haskell.TH.Syntax        (Name)


infix  2 <=>
-- infixl 3 <|>
-- infixl 4 <$>
infixl 4 #
-- infixl 4 <*>
infixl 9 &

(<=>) :: Name -> RHSs a -> Grammar a
name <=> rs = NonTerminal l rs
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
-- -- instance Grammatical (Grammar b) where  type R (Grammar b) = b;  toR = lift
-- instance Grammatical (Grammar b) where
--  type R (Grammar b) = b
--  toR (Terminal s)      = toR s
--  toR (NonTerminal _ r) = r

inject :: Grammar a -> RHSs a
inject (Terminal s)      = terminal s
inject (NonTerminal _ r) = r

terminal :: String -> RHSs a
terminal = lift . Terminal

terminals :: [String] -> RHSs a
terminals = foldr (<|>) empty . map terminal

con :: (Show a) => a -> RHSs a
con c = c <$ (terminal . intercalate " " . unCamelCase . show) c

int :: Int -> RHSs Int
int = con

str :: String -> RHSs String
str s = s <$ terminal s

twig :: (Enum a, Show a) => RHSs a
twig = foldr (<|>) empty . map con $ constructors

