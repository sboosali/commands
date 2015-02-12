{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, RankNTypes #-}
module Commands.Grammar.Types where
import Commands.Etc
import Control.Alternative.Free


data Grammar a
 = Terminal    !String
 | NonTerminal !LHS (RHS a)
 deriving (Functor)

data LHS = LHS !Package !Module !Identifier deriving (Show, Eq, Ord)

type RHS = Alt Grammar

