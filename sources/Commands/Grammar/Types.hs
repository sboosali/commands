{-# LANGUAGE DeriveFunctor, GADTs, PackageImports, RankNTypes #-}
module Commands.Grammar.Types where
import Commands.Command.Types
import Commands.Etc
import Control.Alternative.Free.Tree

import Data.Functor.Constant
import "transformers-compat" Data.Functor.Sum


-- |
data Rule a = Rule !LHS (RHS a)
 deriving (Functor)

-- |
type LHS = GUI
-- data LHS = LHS !Package !Module !Identifier deriving (Show, Eq, Ord)

-- |
--
-- (see <https://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors.html flavours of free applicative functors> for background).
type RHS = Alt Symbol

-- |
type Symbol = Sum (Constant Word) Command

-- |
newtype Word = Word String
 deriving (Show, Eq, Ord)

-- | exhaustive destructor.
--
-- (frees clients from @transformers@ imports. @PatternSynonyms@ in 7.10 can't check exhaustiveness and breaks haddock).
symbol :: (Word -> b) -> (Command a -> b) -> Symbol a -> b
symbol f _ (InL (Constant w)) = f w
symbol _ g (InR r) = g r

-- | constructor.
fromWord :: Word -> Symbol a
fromWord = InL . Constant

-- | constructor.
fromCommand :: Command a -> Symbol a
fromCommand = InR
