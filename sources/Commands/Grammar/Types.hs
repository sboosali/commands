{-# LANGUAGE DeriveFunctor, GADTs, PackageImports, RankNTypes, TypeOperators #-}
module Commands.Grammar.Types where
import Commands.Etc
import Control.Alternative.Free.Tree
import Data.Functor.Constant
import "transformers-compat" Data.Functor.Sum


type (:+:) = Sum

-- |
type Symb = Constant Word :+: Rule

-- |
newtype Word = Word String
 deriving (Show, Eq, Ord)

-- |
data Rule a = Rule !LHS (RHS a)
 deriving (Functor)

-- |
-- data LHS = LHS !Package !Module !Identifier deriving (Show, Eq, Ord)
type LHS = GUI

-- |
--
-- (see <https://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors.html flavours of free applicative functors> for background).
type RHS = Alt Symb

