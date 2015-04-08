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


type (:+:) = Sum

-- | to partially apply a type constructor upon a type argument.
--
-- e.g.
--
-- @('Of' a) :: (* -> *) -> *@
--
-- (read that as "something of @a@")
--
-- it's just backwards type-level application, like @'&' = flip ($)@.
--
-- (@data@ because Haskell can't (?) have a "type-level @Flip@" function as a type alias).
data Of a f = Of (f a)

-- | the "complement" of the 'Rec'ord, where we have one "noun" (i.e. type value) and many "verb"s (i.e. type constructor).
--
-- 
--
-- it's a type constructor, rather than a type alias, letting us partially apply it.
--
-- it flips the type parameters, letting us partially apply on the @fs@
--
newtype RecOf fs a = RecOf { getRec :: Rec (Of a) fs }

-- type Command' (c,xc) g (p,xp) r a = RecOf a [ ... ] -- no: can't pattern match on type aliases
-- type CommandL c xc g p xp =
--  [ "compiler" :::: c xc
--  , "grammar"  :::: Const g
--  , "parser"   :::: p xp
--  , "rhs"      :::: RHS' g p xp
--  , "lhs"      :::: Const LHS
--  ]
type CommandI c xc g p xp = ("compiler" :::: c xc) ': (NonTerminalI g p xp)
type Command' c xc g p xp = RecOf (CommandI c xc g p xp)

-- data RHS'' g p xp a = RHS'' (Alt (Either Terminal (NonTerminal'' g p xp a))) -- no: Either must be fully applied, but Alt takes a partially applied type constructor

-- When LiberalTypeSynonyms is enabled, GHC only type-checks a signature after all type synonyms have been expanded, outermost first. You can now partially apply a type synonym, as long as it's surrounded by another type synonym such that the obvious outermost-first expansion will cause the partially-applied synonym to become fully applied.
-- type Alt'' f a = Alt f a
-- data RHS'' g p xp a = RHS'' (Alt'' (Const Terminal :+: NonTerminal'' g p xp) a)
-- type NonTerminal'' g p xp = RecOf'' -- after all the crazy types, must be a {Rec _ _}
--  [ "grammar"  :::: Const g
--  , "parser"   :::: p xp
--  , "rhs"      :::: RHS'' g p xp
--  ]
-- type RecOf'' fs a = Rec (Of a) fs

data RHS' g p xp a = RHS (Alt (Const Terminal :+: NonTerminal g p xp) a)
type Terminal = String
type NonTerminal g p xp = RecOf (NonTerminalI g p xp)
type NonTerminalI g p xp =
 [ "grammar"  :::: Const g
 , "parser"   :::: p xp
 , "rhs"      :::: RHS' g p xp
 , "lhs"      :::: Const LHS
 -- , "rule"      :::: Rule g p xp self?  -- TODO
 ]

{- I want to be able to mix NonTerminal with Command:
 project a command to a non-terminal e.g.  (&) sugar
 extend a non-terminal with a command e.g.  (<%>)
-}
-- type OSXDGNCommand  a = Command'    Compiler' CompilerContext DNSCommandGrammar Parser' ParserContext a
-- type DGNNonTerminal a = NonTerminal                           DNSCommandGrammar Parser' ParserContext a
type OSXDGNCommand  = Command'    Compiler' CompilerContext DNSCommandGrammar Parser' ParserContext
type DGNNonTerminal = NonTerminal                           DNSCommandGrammar Parser' ParserContext
-- TODO can we support the user easily extending/exchanging the parser/compiler contexts, without hoisting out this extra type everywhere?

exampleCommand :: OSXDGNCommand () -- yes: can totally apply
exampleCommand = undefined
exampleNonTerminal :: DGNNonTerminal () -- yes: can totally apply
exampleNonTerminal = undefined

 -- TODO I don't know if naming the field with a equality constraint affects unification, since field's only used in another constraint
getGrammar
 :: forall fs a g field. (field ~ ("grammar" :::: Const g), field ∈ fs)
 => RecOf fs a -> g
getGrammar (RecOf record) = case rget (Proxy :: Proxy field) record of
 Of (FieldOf (Field (Const g))) -> g
-- getGrammar = getConst . getOf . getField . asField . rget (Proxy :: Proxy ("grammar" :::: Const g)) -- GADTs need case
-- getGrammar = rget (Nothing :: Maybe ("grammar" :::: Const g)) -- a type of kind (* -> *) needs polykinded proxy like Proxy

getParser
 :: forall fs a p xp field. (field ~ ("parser" :::: p xp), field ∈ fs)
 => RecOf fs a -> p xp a
getParser (RecOf record) = case rget (Proxy :: Proxy field) record of
 Of (FieldOf (Field p)) -> p

getCompiler
 :: forall fs a c xc field. (field ~ ("compiler" :::: c xc), field ∈ fs)
 => RecOf fs a -> c xc a
getCompiler (RecOf record) = case rget (Proxy :: Proxy field) record of
 Of (FieldOf (Field c)) -> c

-- | specialized labeled 'Field' for type constructors, for example as elements of a 'RecOf'
--
-- like @:::@, the key is the same but the value is a type constructor that takes one type argument. (Hence the extra @:@? lol.)
data (s :::: f) a = FieldOf (ElField '(s, f a))
-- type k :::: v = ElField '(k,v)

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

