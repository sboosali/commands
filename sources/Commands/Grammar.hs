{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable                 #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving      #-}
{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables                    #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, TypeOperators                #-}
-- |
module Commands.Grammar where
import           Commands.Etc
import           Commands.Grammar.Types
import           Commands.Munging
import           Control.Alternative.Free.Johansen

import           Control.Applicative
import           Control.Monad.Trans.State.Strict
import           Data.Foldable                     (traverse_)
import           Data.List                         (intercalate)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Language.Haskell.TH.Syntax        (Name)
-- import Control.Monad.State hiding  (lift)
import           Control.Monad                     (when)


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

reifyGrammar :: Grammar x -> Map LHS (Some RHSs)
reifyGrammar grammar = execState (reifyGrammar_ grammar) Map.empty

reifyGrammar_ :: Grammar x -> State (Map LHS (Some RHSs)) ()
reifyGrammar_ (Terminal _)       = return ()
reifyGrammar_ (NonTerminal l rs) = do
 traverse_ (\case  (_ `App` g) -> tracing g; _ -> return ()) (alternatives rs)
 visited <- gets $ Map.member l
 when (not visited) $ do
  modify $ Map.insert l (Some rs)
  reifyRHSs_ rs

reifyRHSs_ :: RHSs x -> State (Map LHS (Some RHSs)) ()
reifyRHSs_ (Alt rs) = traverse_ reifyRHS_ rs

reifyRHS_ :: RHS x -> State (Map LHS (Some RHSs)) ()
reifyRHS_ (Pure _) = return ()
reifyRHS_ (rs `App` g) = do
 reifyGrammar_ g
 reifyRHSs_ rs
