{-# LANGUAGE DeriveFunctor, DeriveGeneric, GADTs, PackageImports, RankNTypes #-}
module Commands.Grammar.Types where
import Commands.Command.Types            ()
import Commands.Etc
import Commands.Frontends.Dragon13.Types
import Commands.Parse.Types
import Control.Alternative.Free.Tree

import Data.Functor.Constant
import "transformers-compat" Data.Functor.Sum
import Data.Hashable                     (Hashable)
import GHC.Generics                      (Generic)


-- |
--
-- Command ~ LHS * DNSGrammar Text Text * Parser a
--
-- RHS ~ Alt Symbol ~ Constant Word + Command ~ Constant Word + (LHS * DNSGrammar Text Text * Parser a)
--
data Command a = Command
 { _lhs     :: !LHS
 -- , _rule     :: Rule a
 , _grammar :: DNSGrammar DNSCommandName String
 , _parser  :: Parser a
 }
 deriving (Functor)

-- | 'String' because user-facing "config" modules (e.g.
-- "Commands.Plugins.Example") can't use:
--
-- * both OverloadedStrings
-- * and type class sugar
--
--
type DNSCommandName = String -- LHS

-- |
data Rule a = Rule !LHS (RHS a)
 deriving (Functor)

-- |
data LHS
 = LHS    !GUI                  -- ^ for tokens guaranteed unique by Haskell's name resolution modulo package
 | LHSInt !Int                  -- ^ for tokens guaranteed unique by safe/monadic generation
 | LHSApp !LHS ![LHS]           -- ^ for reifying @app@lication of higher-order 'Rule's like @multiple@
 deriving (Show, Eq, Ord, Generic)
instance Hashable LHS

-- |
--
-- (see <https://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors.html flavours of free applicative functors> for background).
--
-- 'RHS's must never be recursively defined, only 'Command's can be
-- recursively defined. 'Command's are like 'RHS's tagged with unique
-- 'LHS's, providing a token to keep track of during evaluation
-- (like "searching a graph").
-- Some functions on 'RHS's assume non-recursive 'RHS's, just
-- like @('length' :: [a] -> Int)@ assumes non-recursive @[a]@.
-- ("data not codata"?)
--
--
type RHS = Alt Symbol

-- |
type Symbol = Sum (Constant Word) Command

-- |
newtype Word = Word { unWord :: String }
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

liftCommand :: Command a -> RHS a
liftCommand = lift . fromCommand

liftString :: String -> RHS a
liftString = lift . fromWord . Word
