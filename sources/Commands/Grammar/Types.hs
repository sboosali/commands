{-# LANGUAGE DataKinds, DeriveFunctor, DeriveGeneric, FlexibleContexts  #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures          #-}
{-# LANGUAGE NamedFieldPuns, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeOperators                             #-}
module Commands.Grammar.Types where
-- import Commands.Command.Types
import Commands.Backends.OSX.Types         (Actions, Application)
import Commands.Etc
import Commands.Frontends.Dragon13.Lens
import Commands.Frontends.Dragon13.Types
import Commands.Parse.Types
import Control.Alternative.Free.Associated

import Control.Lens
import Data.Hashable                       (Hashable)
import Numeric.Natural                     (Natural)

import Data.Functor.Sum
import GHC.Generics                        (Generic)


type (:+:) = Sum

data Command a = Command
 { _comGrammar  :: Grammar a
 , _comCompiler :: Compiler a
 }
-- TODO  profunctor? does it matter? probably, I like thinking about it like a class: a class method to introduce, an instance method to eliminate.

-- newtype Compiler' a = Compiler' (a -> ReaderT CompilerContext Actions ())
newtype Compiler' x a = Compiler' (Compiler a)
 -- deriving (Contravariant, Divisible?, Monad??)

newtype Parser' x a = Parser' (Parser a)

-- |
type Compiler a = a -> CompilerContext -> Actions ()
-- the unit (Actions ()) may cause hard to read type errors when using "copy", but avoids existential quantification.
-- can cache, when both arguments instantiate Eq?

type CompilerContext = Application

-- TODO lol
globalContext :: CompilerContext
globalContext = ""


-- |
--
-- Grammar ~ LHS * DNSGrammar Text Text * Parser a
--
-- RHS ~ Alt Symbol ~ Const Word + Grammar ~ Const Word + (LHS * DNSGrammar Text Text * Parser a)
--
data Grammar a = Grammar
 { _gramRule    :: Rule a            -- ^
 , _gramGrammar :: DNSCommandGrammar -- ^
 , _gramParser  :: Parser a          -- ^
 }
 deriving (Functor)


type DNSCommandGrammar = DNSCommandProduction
 -- DNSGrammar DNSInfo DNSCommandName DNSCommandToken

type DNSCommandProduction = DNSProduction DNSInfo DNSCommandName DNSCommandToken

defaultDNSCommandProduction :: LHS -> DNSCommandRHS -> DNSCommandProduction
defaultDNSCommandProduction l r = DNSProduction defaultDNSInfo (DNSRule (defaultDNSExpandedName l)) r

type DNSCommandRHS = DNSRHS DNSCommandName DNSCommandToken

type DNSCommandName = DNSExpandedName LHS

-- | not 'Text' because user-facing "config" modules (e.g.
-- "Commands.Plugins.Example") can only use one of:
--
-- * OverloadedStrings
-- * type class sugar
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

-- | unary/curried application. a specialized 'LHSApp' for convenience.
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
type RHS = Alter GrammaticalSymbol

-- {- |

-- the type parameters are named:

-- * @d@ for the desugarer
-- * @g@ for the reified grammar
-- * @p@ for the parser

-- -}
-- type RHS d g p = Alt (Symbol d g p)
-- data Symbol d g p a = Word String | Rule (Grammar d g p a) deriving Functor
-- rule :: Grammar a -> RHS a
-- rule = liftAlt . Rule
-- word :: String -> RHS a
-- word = liftAlt . Word

-- |
type GrammaticalSymbol = Sum (Const Terminal) Grammar

-- |
newtype Terminal = Terminal { unTerminal :: String }
 deriving (Show, Eq, Ord)

-- | exhaustive destructor.
--
-- (frees clients from @transformers@ imports. @PatternSynonyms@ in 7.10 can't check exhaustiveness and breaks haddock).
symbol :: (String -> b) -> (Grammar a -> b) -> GrammaticalSymbol a -> b
symbol f _ (InL (Const (Terminal s))) = f s
symbol _ g (InR r) = g r

-- | constructor.
fromWord :: Terminal -> GrammaticalSymbol a
fromWord = InL . Const

-- | constructor.
fromGrammar :: Grammar a -> GrammaticalSymbol a
fromGrammar = InR

liftGrammar :: Grammar a -> RHS a
liftGrammar = liftAlter . fromGrammar
-- TODO rename to rule

liftString :: String -> RHS a
liftString = liftAlter . fromWord . Terminal
-- TODO rename to word

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



makeLenses ''Grammar
makeLenses ''Command
makeLenses ''Rule

makeLenses ''DNSExpandedName
makeLenses ''DNSInfo


gramLHS :: Lens' (Grammar a) LHS
gramLHS = gramRule . ruleLHS

gramRHS :: Lens' (Grammar a) (RHS a)
gramRHS = gramRule . ruleRHS

gramExpand :: Lens' (Grammar a) Natural
gramExpand = gramGrammar.dnsProductionInfo.dnsExpand

gramInline :: Lens' (Grammar a) Bool
gramInline = gramGrammar.dnsProductionInfo.dnsInline

