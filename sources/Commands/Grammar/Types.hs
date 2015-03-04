{-# LANGUAGE DeriveFunctor, GADTs, PackageImports, RankNTypes #-}
module Commands.Grammar.Types where
import Commands.Etc
import Control.Alternative.Free.Tree

import Data.Functor.Constant
import "transformers-compat" Data.Functor.Sum


-- |
type Symbol = Sum (Constant Word) Rule

-- | exhaustive destructor.
--
-- (frees clients from @transformers@ imports. @PatternSynonyms@ in 7.10 can't check exhaustiveness and break haddock).
symbol :: (Word -> b) -> (Rule a -> b) -> Symbol a -> b
symbol f _ (InL (Constant w)) = f w
symbol _ g (InR r) = g r

-- | constructor.
fromWord :: Word -> Symbol a
fromWord = InL . Constant

-- | constructor.
fromRule :: Rule a -> Symbol a
fromRule = InR

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
type RHS = Alt Symbol

