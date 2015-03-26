{-# LANGUAGE DataKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs         #-}
{-# LANGUAGE KindSignatures, LambdaCase, NamedFieldPuns, RankNTypes      #-}
{-# LANGUAGE StandaloneDeriving, ViewPatterns                            #-}
module Commands.Frontends.Dragon13.Types where
import Commands.Etc

import Control.Applicative
import Control.Lens.Plated (Plated (..))
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Char           (toLower)
import Data.Foldable
import Data.List.NonEmpty  (NonEmpty (..))
import Data.Traversable
import Prelude             hiding (mapM)
import GHC.Exts ( IsString(..) )


-- ================================================================ --

-- | a Dragon NaturallySpeaking (\"DNS") grammar has
-- one or more 'DNSProduction's, exactly one of which is an export.
-- (the "exactly one" constraint should really be "at least one", but
-- this restriction shouldn't matter).
--
-- in the type @DNSGrammar i n t@, you can read:
--
-- * @i@ as @info@
-- * @n@ as @name@ or @nonTerminal@
-- * @t@ as @text@ or @terminal@
--
-- a 'Bitraversable', for easy validation
-- (e.g. "Commands.Frontends.Dragon13.escapeDNSGrammar")
-- not a 'Traversable', because the terminals and the non-terminals
-- are distinct lexemes, with different criteria for being valid.
--
--
-- in relation to NatLink's concrete syntax:
--
-- * @"\<rule> = ...;"@ and @"\<rule> exported = ...;"@ both declare a @'DNSProduction' ...@
-- * @"self.setList('list', [...])"@ declares a 'DNSVocabulary'
-- * @"\<rule> imported;"@ is a 'DNSImport'
--
--
-- a 'DNSVocabulary' can be named only by 'LHSList's, as
-- a 'DNSProduction' can be named only by 'LHSRule's.
--
-- an 'LHSRule' \'s 'DNSRHS' must be 'NonEmpty', but an 'LHSList' \'s 'DNSToken's may be empty.
--
data DNSGrammar i n t = DNSGrammar
 { _dnsProductions  :: NonEmpty (DNSProduction i n t)
 , _dnsVocabularies :: [DNSVocabulary i n t]
 , _dnsImports      :: [DNSImport n] -- TODO  a Set, or remove duplicates later
 }
 deriving (Show, Eq)

-- TODO instance Semigroup (DNSGrammar n t) where <>

instance Bifunctor     (DNSGrammar i) where  bimap     = bimapDefault
instance Bifoldable    (DNSGrammar i) where  bifoldMap = bifoldMapDefault
instance Bitraversable (DNSGrammar i) where -- valid Bitraversable?
 bitraverse f g (DNSGrammar productions vocabularies imports) = DNSGrammar
  <$> traverse (bitraverse f g) productions
  <*> traverse (bitraverse f g) vocabularies
  <*> traverse (traverse f) imports

-- |
--
-- you can only import 'LHSRule's.
type DNSImport n = DNSLHS LHSRule n

-- | import every 'DNSBuiltinRule'.
--
dnsHeader :: [DNSImport n]
dnsHeader = DNSBuiltinRule <$> constructors


-- ================================================================ --

-- |
--
--
data DNSProduction i n t = DNSProduction
 { _dnsProductionInfo :: i
 , _dnsProductionLHS  :: (DNSLHS LHSRule n)
 , _dnsProductionRHS  :: (DNSRHS n t)
 }
 deriving (Show,Eq,Ord)

instance Bifunctor     (DNSProduction i) where bimap     = bimapDefault
instance Bifoldable    (DNSProduction i) where bifoldMap = bifoldMapDefault
instance Bitraversable (DNSProduction i) where
 bitraverse f g (DNSProduction i l rs) = DNSProduction i <$> traverse f l <*> bitraverse f g rs

-- |
--
--
--
data DNSVocabulary i n t = DNSVocabulary
 { _dnsVocabularyInfo   :: i
 , _dnsVocabularyLHS    :: (DNSLHS LHSList n)
 , _dnsVocabularyTokens :: [DNSToken t]
 }
 deriving (Show,Eq,Ord)

instance Bifunctor     (DNSVocabulary i) where bimap     = bimapDefault
instance Bifoldable    (DNSVocabulary i) where bifoldMap = bifoldMapDefault
instance Bitraversable (DNSVocabulary i) where
 bitraverse f g (DNSVocabulary i l ts) = DNSVocabulary i <$> traverse f l <*> traverse (traverse g) ts


-- ================================================================ --

-- | the
-- <https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form EBNF>-like
-- grammar specification.
--
-- 'Eq' instance is manual because a constructor ('DNSNonTerminal') is existentially-quantified.
data DNSRHS n t
 = DNSTerminal    (DNSToken t) -- ^ e.g. @"terminal"@
 | DNSNonTerminal (SomeDNSLHS n) -- ^ e.g. @\<non_terminal>@ or @{non_terminal}@
 | DNSOptional (DNSRHS n t) -- ^ e.g. @[optional]@
 | DNSMultiple (DNSRHS n t) -- ^ e.g. @(multiple)+@
 | DNSSequence     (NonEmpty (DNSRHS n t)) -- ^ e.g. @first second ...@
 | DNSAlternatives (NonEmpty (DNSRHS n t)) -- ^ e.g. @(alternative | ...)@
 deriving (Show,Eq,Ord)

instance Bifunctor     DNSRHS where bimap     = bimapDefault
instance Bifoldable    DNSRHS where bifoldMap = bifoldMapDefault
instance Bitraversable DNSRHS where -- valid Bitraversable?
 bitraverse _ g (DNSTerminal t)      = DNSTerminal     <$> traverse g t
 bitraverse f _ (DNSNonTerminal n)   = DNSNonTerminal  <$> traverse f n
 bitraverse f g (DNSSequence rs)     = DNSSequence     <$> traverse (bitraverse f g) rs
 bitraverse f g (DNSAlternatives rs) = DNSAlternatives <$> traverse (bitraverse f g) rs
 bitraverse f g (DNSOptional r)      = DNSOptional     <$> bitraverse f g r
 bitraverse f g (DNSMultiple r)      = DNSMultiple     <$> bitraverse f g r

instance Plated (DNSRHS n t) where
 plate f (DNSOptional r)      = DNSOptional     <$> f r
 plate f (DNSMultiple r)      = DNSMultiple     <$> f r
 plate f (DNSSequence rs)     = DNSSequence     <$> traverse f rs -- TODO is the instance correct? Are these "immediate" children? they are non-transitive, and seem necessary for the use I want
 plate f (DNSAlternatives rs) = DNSAlternatives <$> traverse f rs
 plate _ r = pure r

-- | for readable @doctest@s 
instance (IsString t) => (IsString (DNSRHS n t)) where
 fromString = DNSTerminal . DNSToken . fromString

-- | the "additive identity" i.e.:
--
-- * identity to 'DNSAlternatives'
-- * annihilator to 'DNSSequence'
--
-- ("verified" by experimenting with Dragon NaturallySpeaking)
--
-- these properties are used at different stages of building the
-- grammar (e.g. "Commands.Frontends.Dragon13.Render" and
-- "Commands.Frontends.Dragon13.Optimize"). these stages use different
-- name types (i.e. the @n@ in @DNSRHS n t@): not the same one, and
-- not just Strings. Thus, we need a parametrically polymorphic
-- 'zeroDNSRHS':
--
-- @
-- zeroDNSRHS = DNSNonTerminal (SomeDNSLHS (DNSBuiltinList 'DNSEmptyList')) :: DNSRHS n t
-- @
--
-- rather than the simple but non-@n@-polymorphic:
--
--
-- @
-- zeroDNSRHS = DNSNonTerminal (SomeDNSLHS (DNSList "emptyList")) :: DNSRHS String t
-- @
--
--
zeroDNSRHS :: DNSRHS n t
zeroDNSRHS = DNSNonTerminal (SomeDNSLHS (DNSBuiltinList DNSEmptyList))

-- | the "multiplicative identity":
--
-- * identity to 'DNSSequence'
--
-- ("verified" by experimenting with Dragon NaturallySpeaking)
--
--  Dragon NaturallySpeaking's grammatical format is EBNF-like, but there's no "epsilon production" (i.e. the production that matches the empty string, always succeeding). I tried @'DNSToken' ""@, but that raised an error. with a "epsilon" primitive, it seems to me easy to implement the Multiple:
--
-- @<multiple_x> = <eps> | x <multiple_x>@
--
-- and Optional:
--
-- @<optional_x> = <eps> | x@
--
-- productions. abusing notation, let's rewrite the higher-order
-- 'DNSOptional' production as:
--
-- @optional(x) = 1 + x@
--
-- and the 'zeroDNSRHS' as:
--
-- @0@
--
-- we want @1@, we only have @0@ and @optional(x)@. so:
--
-- @1 = 1 + 0 = optional(0)@
--
-- by:
--
-- * additive identity of @0@ and
-- * the definition of @optional(x)@.
--
-- thus:
--
-- @unitDNSRHS = 'DNSOptional' 'zeroDNSRHS'@
--
-- denotationally, we have equational reasoning. operationally, I guess that DNS checks whether it can match the current token to any token in the empty list, which it never can (that fails), but then the option is matched (always succeeds).
--
unitDNSRHS :: DNSRHS n t
unitDNSRHS = DNSOptional zeroDNSRHS

nonemptyDNSRHS :: DNSRHS n t -> NonEmpty (DNSRHS n t)
nonemptyDNSRHS (DNSAlternatives rs) = rs
nonemptyDNSRHS r = r :| []


-- ================================================================ --

-- |
--
-- in the type @DNSLHS l n@, you can read:
--
-- * @l@ as @leftHandSide@
-- * @n@ as @name@ or @nonTerminal@
--
-- in relation to NatLink's concrete syntax:
--
-- * @"\<rule>"@ is a 'DNSRule'
-- * @"\<dgndictation>"@ is a 'DNSBuiltinRule'
-- * @"{list}@ is a 'DNSList'
--
-- a @GADT@ to distinguish 'LHSRule's from 'LHSList's, which behave
-- differently. without @GADT@s, we would need sacrifice
-- either safety (by not distinguishing things that are distinct)
-- or readability (by wrapping each distinction in its own type).
--
-- the instances are manual because of the error:
--
-- @
-- Can't make a derived instance of ...:
-- Constructor ... must not have existential arguments
-- @
--
-- (see <https://ghc.haskell.org/trac/ghc/ticket/8678>)
--
data DNSLHS (l :: LHSKind) n where
 DNSRule        :: n              -> DNSLHS LHSRule n
 DNSBuiltinRule :: DNSBuiltinRule -> DNSLHS LHSRule x
 DNSList        :: n              -> DNSLHS LHSList n
 DNSBuiltinList :: DNSBuiltinList -> DNSLHS LHSList x

-- TODO enforce whether DNSLHS, well, whether it can be on the LHS rather than only in an RHS.

deriving instance (Show n) => Show (DNSLHS l n)
instance (Eq n) => Eq (DNSLHS l n) where (==) = equalDNSLHS
instance (Ord n) => Ord (DNSLHS l n) where compare = compareDNSLHS

instance Functor     (DNSLHS l) where fmap     = fmapDefault
instance Foldable    (DNSLHS l) where foldMap  = foldMapDefault
instance Traversable (DNSLHS l) where
 traverse f (DNSRule n) = DNSRule <$> f n
 traverse f (DNSList n) = DNSList <$> f n
 traverse _ (DNSBuiltinRule x) = pure (DNSBuiltinRule x)
 traverse _ (DNSBuiltinList x) = pure (DNSBuiltinList x)

-- | heterogeneous (but only in the phantom) equality
equalDNSLHS :: (Eq n) => DNSLHS l1 n -> DNSLHS l2 n -> Bool
DNSRule        x `equalDNSLHS` DNSRule        y = x == y
DNSBuiltinRule x `equalDNSLHS` DNSBuiltinRule y = x == y
DNSList        x `equalDNSLHS` DNSList        y = x == y
DNSBuiltinList x `equalDNSLHS` DNSBuiltinList y = x == y
_                `equalDNSLHS` _                = False

-- | heterogeneous (wrt the phantom) comparison
compareDNSLHS :: (Ord n) => DNSLHS l1 n -> DNSLHS l2 n -> Ordering
DNSRule        x  `compareDNSLHS` DNSRule        y  = x `compare` y
DNSBuiltinRule x  `compareDNSLHS` DNSBuiltinRule y  = x `compare` y
DNSList        x  `compareDNSLHS` DNSList        y  = x `compare` y
DNSBuiltinList x  `compareDNSLHS` DNSBuiltinList y  = x `compare` y
(rankDNSLHS -> x) `compareDNSLHS` (rankDNSLHS -> y) = x `compare` y

rankDNSLHS :: DNSLHS l n -> Integer
rankDNSLHS = \case
 DNSRule        {} -> 0
 DNSBuiltinRule {} -> 1
 DNSList        {} -> 2
 DNSBuiltinList {} -> 3

-- | Builtin 'DNSProduction's: they have left-hand sides,
-- but they don't have right-hand sides.
data DNSBuiltinRule = DGNDictation | DGNWords | DGNLetters
 deriving (Show, Eq, Ord, Enum)

displayDNSBuiltinRule :: DNSBuiltinRule -> String
displayDNSBuiltinRule = fmap toLower . show

-- | Builtin 'DNSVocabulary's.
--
data DNSBuiltinList = DNSEmptyList
 deriving (Show, Eq, Ord, Enum)

displayDNSBuiltinList :: DNSBuiltinList -> String
displayDNSBuiltinList DNSEmptyList = "emptyList"

-- | for promotion by @DataKinds@.
--
-- 'LHSRule's and 'LHSList's seem to share different namespaces
-- in Dragon NaturallySpeaking.
--
data LHSKind = LHSRule | LHSList


-- ================================================================ --

-- |
data SomeDNSLHS n = forall l. SomeDNSLHS (DNSLHS l n)

instance (Show n) => Show (SomeDNSLHS n) where
 showsPrec d (SomeDNSLHS l) = showParen (d > 10)
  (showString "SomeDNSLHS " . showsPrec (10+1) l)

instance (Eq   n) => Eq   (SomeDNSLHS n) where SomeDNSLHS l1 == SomeDNSLHS l2 = l1 `equalDNSLHS` l2
instance (Ord  n) => Ord  (SomeDNSLHS n) where SomeDNSLHS l1 `compare` SomeDNSLHS l2 = l1 `compareDNSLHS` l2

instance Functor     SomeDNSLHS where fmap     = fmapDefault
instance Foldable    SomeDNSLHS where foldMap  = foldMapDefault
instance Traversable SomeDNSLHS where traverse f (SomeDNSLHS l) = SomeDNSLHS <$> traverse f l


-- ================================================================ --

-- | the "leaves" of the grammar.
data DNSToken t
 = DNSToken t -- ^ e.g. @"word or phrase"@
 | DNSPronounced t t -- ^ e.g. @written\\spoken@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | for readable @doctest@s 
instance (IsString t) => (IsString (DNSToken t)) where
 fromString = DNSToken . fromString

