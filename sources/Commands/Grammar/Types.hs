{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, RankNTypes #-}
module Commands.Grammar.Types where
import Commands.Etc
import Control.Alternative.Free


-- |
data Grammar a
 = Terminal    !String
 | NonTerminal !LHS (RHS a)
 deriving (Functor)

-- |
data LHS = LHS !Package !Module !Identifier deriving (Show, Eq, Ord)

-- |
--
-- (see <https://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors.html flavours of free applicative functors> for background).
type RHS = Alt Grammar

