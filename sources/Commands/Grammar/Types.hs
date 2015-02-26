{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE RankNTypes, StandaloneDeriving                            #-}
module Commands.Grammar.Types where
import Commands.Etc
import Control.Alternative.Free.Johansen


-- |
data Grammar a
 = Terminal    !String
 | NonTerminal !LHS (RHSs a)
 deriving (Functor)

-- |
-- data LHS = LHS !Package !Module !Identifier deriving (Show, Eq, Ord)
type LHS = GUI

-- |
--
-- (see <https://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors.html flavours of free applicative functors> for background).
type RHS = App Grammar

type RHSs = Alt Grammar

-- TODO remove this abomination
instance Show (Grammar a) where
 show (Terminal s) = "Terminal " ++ s
 show (NonTerminal l _) = "NonTerminal (" ++ show l ++ ") _"

