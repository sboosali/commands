{-# LANGUAGE DeriveFunctor, DeriveGeneric, NamedFieldPuns, PackageImports #-}
{-# LANGUAGE RankNTypes, TemplateHaskell                                  #-}
module Commands.Grammar.Types where
import Commands.Command.Types            ()
import Commands.Etc
import Commands.Frontends.Dragon13.Lens
import Commands.Frontends.Dragon13.Types
import Commands.Parse.Types
import Control.Alternative.Free.Tree

import Control.Lens
import Data.Functor.Constant
import "transformers-compat" Data.Functor.Sum
import Data.Hashable                     (Hashable)
import GHC.Generics                      (Generic)
import Numeric.Natural                   (Natural)


-- |
--
-- Command ~ LHS * DNSGrammar Text Text * Parser a
--
-- RHS ~ Alt Symbol ~ Constant Word + Command ~ Constant Word + (LHS * DNSGrammar Text Text * Parser a)
--
data Command a = Command
 { _comRule    :: Rule a
 , _comGrammar :: DNSCommandGrammar
 , _comParser  :: Parser a
 }
 deriving (Functor)

type DNSCommandGrammar = DNSCommandProduction
 -- DNSGrammar DNSInfo DNSCommandName DNSCommandToken

type DNSCommandProduction = DNSProduction DNSInfo DNSCommandName DNSCommandToken

type DNSCommandRHS = DNSRHS DNSCommandName DNSCommandToken

type DNSCommandName = DNSExpandedName LHS

-- | not 'Text' because user-facing "config" modules (e.g.
-- "Commands.Plugins.Example") can't use:
--
-- * both OverloadedStrings
-- * and type class sugar
--
type DNSCommandToken = String


-- |
data Rule a = Rule
 { _ruleLHS :: !LHS
 , _ruleRHS :: RHS a
 }
 deriving (Functor)

-- | mono in the first, poly in the second.
bimapRule :: (LHS -> LHS) -> (RHS a -> RHS b) -> Rule a -> Rule b
bimapRule f g (Rule l r) = Rule (f l) (g r)

-- |
data LHS
 = LHS    !GUI                  -- ^ for tokens guaranteed unique by Haskell's name resolution modulo package
 | LHSInt !Int                  -- ^ for tokens guaranteed unique by safe/monadic generation
 | LHSApp !LHS [LHS]            -- ^ for reifying @app@lication of higher-order 'Rule's like @multiple@
 deriving (Show, Eq, Ord, Generic)
instance Hashable LHS

-- | specialized 'LHSApp', for convenience.
appLHS :: LHS -> LHS -> LHS
appLHS lhs = (lhs `LHSApp`) . (:[])

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


-- | a name, with the level of its expansion.
--
-- '_dnsExpansion' tracks which level a recursive 'DNSProduction' has been expanded to.
--
-- when the '_dnsExpansion' is @Nothing@, the 'DNSProduction' will not be expanded.
--
-- when the '_dnsExpansion' is @Just 0@, the 'DNSProduction' will only
-- hold base case 'DNSAlternative's, not the recursive 'DNSAlternative's.
--
data DNSExpandedName n = DNSExpandedName
 { _dnsExpansion    :: Maybe Natural
 , _dnsExpandedName :: n
 }
 deriving (Show,Eq,Ord,Functor)

-- | yet un-expanded
defaultDNSExpandedName :: n -> DNSExpandedName n
defaultDNSExpandedName = DNSExpandedName Nothing

-- | metadata to properly transform a 'DNSGrammar' into one that Dragon NaturallySpeaking accepts.
--
--
data DNSInfo = DNSInfo
 { _dnsExpand :: !Natural -- ^ how many times to expand a recursive 'DNSProduction'
 , _dnsInline :: !Bool    -- ^ whether or not to inline a 'DNSProduction'
 }
 deriving (Show,Eq,Ord)

-- | no expansion and no inlining.
defaultDNSInfo :: DNSInfo
defaultDNSInfo = DNSInfo 0 False



makeLenses ''Command
makeLenses ''Rule

makeLenses ''DNSExpandedName
makeLenses ''DNSInfo


comLHS :: Lens' (Command a) LHS
comLHS = comRule . ruleLHS

comRHS :: Lens' (Command a) (RHS a)
comRHS = comRule . ruleRHS

comExpand :: Lens' (Command a) Natural
comExpand = comGrammar.dnsProductionInfo.dnsExpand

comInline :: Lens' (Command a) Bool
comInline = comGrammar.dnsProductionInfo.dnsInline
