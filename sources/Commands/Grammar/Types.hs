{-# LANGUAGE DeriveFunctor, DeriveGeneric, NamedFieldPuns, PackageImports #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, TypeOperators, DataKinds, PolyKinds, GeneralizedNewtypeDeriving, KindSignatures, FlexibleContexts, ScopedTypeVariables, GADTs                               #-}
module Commands.Grammar.Types where
-- import Commands.Command.Types
import Commands.Backends.OSX.Types       (Actions, Application)
import Commands.Etc
import Commands.Frontends.Dragon13.Lens
import Commands.Frontends.Dragon13.Types
import Commands.Parse.Types
import Control.Alternative.Free.Tree

import Control.Lens
import Data.Hashable                     (Hashable)
import Numeric.Natural                   (Natural)
import Data.Vinyl

import Data.Functor.Constant
import "transformers-compat" Data.Functor.Sum
import GHC.Generics                      (Generic)
import Data.Proxy (Proxy(..))
-- import           Control.Applicative
-- import Control.Monad.Trans.Reader
-- import GHC.TypeLits (Symbol)


-- | to partially apply a type constructor upon a type argument.
--
-- e.g.
--
-- @('Of' a) :: (* -> *) -> *@
--
-- backwards application, as '&' to '$'.
data Of a f = Of (f a)

type RecOf a fs = Rec (Of a) fs

-- type C a = Rec (Of a) ["grammar" :$: Grammar, "compiler" :$: Compiler']
-- type C a = RecOf a ["compiler" :$: Compiler', "rule" :$: Rule, "grammar" :$: Const DNSCommandGrammar, "parser" :$: Parser']
-- type Command' c g p r a = RecOf a
--  [ "compiler" :$: c
--  , "grammar"  :$: Const g
--  , "parser"   :$: p
--  , "rule"     :$: r
--  ]
type Command' c xc g p xp r a = RecOf a
 [ "compiler" :$: c xc
 , "grammar"  :$: Const g
 , "parser"   :$: p xp
 , "rule"     :$: r
 ]

type (:+:) = Sum
-- type RHS' = Alt GrammaticalSymbol'
-- type GrammaticalSymbol' = Const Terminal :+: NonTerminal
type RHS' = Alt (Const Terminal :+: NonTerminal)
data NonTerminal a = NonTerminal
type Terminal = ()

type DGNOSXCommand a = Command' Compiler' CompilerContext DNSCommandGrammar Parser' ParserContext Rule a

type G a = RecOf a ["rule" :$: Rule, "grammar" :$: Const DNSCommandGrammar, "parser" :$: Parser' ParserContext]


getGrammar
 :: forall a fs g field. (field ~ ("grammar" :$: Const g), field ∈ fs)
 => RecOf a fs -> g
getGrammar record = case rget (Proxy :: Proxy field) record of
 Of (FieldOf (Field (Const g))) -> g
-- getGrammar = getConst . getOf . getField . asField . rget (Proxy :: Proxy ("grammar" :$: Const g)) -- GADTs need case
-- getGrammar = rget (Nothing :: Maybe ("grammar" :$: Const g)) -- a type of kind (* -> *) needs polykinded proxy like Proxy

getParser
 :: forall a fs p xp field. (field ~ ("parser" :$: p xp), field ∈ fs)
 => RecOf a fs -> p xp a
getParser record = case rget (Proxy :: Proxy field) record of
 Of (FieldOf (Field p)) -> p

getCompiler
 :: forall a fs c xc field. (field ~ ("compiler" :$: c xc), field ∈ fs)
 => RecOf a fs -> c xc a
getCompiler record = case rget (Proxy :: Proxy field) record of
 Of (FieldOf (Field c)) -> c

-- type k :$: v = ElField '(k,v)
data (s :$: f) a = FieldOf (ElField '(s, f a))

data Command a = Command
 { _comGrammar  :: Grammar a
 , _comCompiler :: Compiler a
 }
-- TODO  profunctor? does it matter?

-- newtype Compiler' a = Compiler' (a -> ReaderT CompilerContext Actions ())
newtype Compiler' x a = Compiler' (Compiler a)
 -- deriving (Contravariant, Divisible?, Monad??)

newtype Parser' x a = Parser' (Parser a)

-- |
type Compiler a = a -> CompilerContext -> Actions ()
-- the unit (Actions ()) may cause hard to read type errors when using "copy", but avoids existential quantification
-- can cache, when both arguments instantiate Eq?

type CompilerContext = Application

-- TODO lol
globalContext :: CompilerContext
globalContext = ""


-- |
--
-- Grammar ~ LHS * DNSGrammar Text Text * Parser a
--
-- RHS ~ Alt Symbol ~ Constant Word + Grammar ~ Constant Word + (LHS * DNSGrammar Text Text * Parser a)
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
type RHS = Alt GrammaticalSymbol

-- |
type GrammaticalSymbol = Sum (Constant Word) Grammar

-- |
newtype Word = Word { unWord :: String }
 deriving (Show, Eq, Ord)

-- | exhaustive destructor.
--
-- (frees clients from @transformers@ imports. @PatternSynonyms@ in 7.10 can't check exhaustiveness and breaks haddock).
symbol :: (Word -> b) -> (Grammar a -> b) -> GrammaticalSymbol a -> b
symbol f _ (InL (Constant w)) = f w
symbol _ g (InR r) = g r

-- | constructor.
fromWord :: Word -> GrammaticalSymbol a
fromWord = InL . Constant

-- | constructor.
fromGrammar :: Grammar a -> GrammaticalSymbol a
fromGrammar = InR

liftGrammar :: Grammar a -> RHS a
liftGrammar = lift . fromGrammar

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

