{-# LANGUAGE DataKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs         #-}
{-# LANGUAGE KindSignatures, RankNTypes, StandaloneDeriving              #-}
module Commands.Frontends.Dragon13.Types where
import Commands.Etc        ()
import Commands.Instances  ()

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


-- ================================================================ --

-- |
--
-- in relation to NatLink's concrete syntax:
--
-- * @"\<rule> imported;"@ is a 'DNSImport'
--
type DNSImport n = DNSLHS LHSRule n

-- | a Dragon NaturallySpeaking (\"DNS") grammar has
-- one or more 'DNSProduction's, exactly one of which is an export.
-- (the "exactly one" constraint should really be "at least one", but
-- this restriction shouldn't matter).
--
-- in the type @DNSGrammar n t@, you can read:
--
-- * @n@ as @name@ or @nonTerminal@
-- * @t@ as @text@ or @terminal@
--
-- a 'Bitraversable', for easy validation
-- (e.g. "Commands.Frontends.Dragon13.escapeDNSGrammar")
-- not a @Traversable@, because the terminals and the non-terminals
-- are distinct lexemes, with different criteria for being valid.
--
data DNSGrammar n t = DNSGrammar
 { _dnsExport      :: DNSProduction True n t
 , _dnsImports     :: [DNSImport n]
 , _dnsProductions :: [DNSProduction False n t]
 }
 deriving (Show, Eq)

instance Bifunctor     DNSGrammar where  bimap     = bimapDefault
instance Bifoldable    DNSGrammar where  bifoldMap = bifoldMapDefault
instance Bitraversable DNSGrammar where -- valid Bitraversable?
 bitraverse f g (DNSGrammar export imports productions) = DNSGrammar
  <$> bitraverse f g export
  <*> traverse (traverse f) imports
  <*> traverse (bitraverse f g) productions


-- ================================================================ --

-- | The top-level statements of a grammar.
--
-- in the type @DNSProduction e n t@, you can read:
--
-- * @e@ as @isExported@
-- * @n@ as @name@ or @nonTerminal@
-- * @t@ as @text@ or @terminal@
--
-- in relation to NatLink's concrete syntax:
--
-- * @"\<rule> = ...;"@ has type @'DNSProduction' 'False' ...@
-- * @"\<rule> exported = ...;"@ has type @'DNSProduction' 'True' ...@
-- * @"self.setList('list', [...])"@ declares a 'DNSVocabulary'
--
-- a @GADT@ to constrain the exportability and the right-hand sides of
-- its constructors.
-- you can import and export only 'DNSRule's (including 'DNSBuiltinRule's),
-- not 'DNSList's. a 'DNSVocabulary' can be named only by 'LHSList's, as
-- a 'DNSProduction' can be named only by 'LHSRule's.
--
-- an 'LHSRule' \'s 'DNSRHS' must be 'NonEmpty', but an 'LHSList' \'s 'DNSToken's may be empty.
--
data DNSProduction (e :: Bool) n t where
 DNSProduction :: DNSLHS LHSRule n -> NonEmpty (DNSRHS n t) -> DNSProduction e     n t
 DNSVocabulary :: DNSLHS LHSList n -> [DNSToken t]          -> DNSProduction False n t

deriving instance (Show n, Show t) => Show (DNSProduction e n t)
deriving instance (Eq   n, Eq   t) => Eq   (DNSProduction e n t)

instance Bifunctor     (DNSProduction e) where bimap     = bimapDefault
instance Bifoldable    (DNSProduction e) where bifoldMap = bifoldMapDefault
instance Bitraversable (DNSProduction e) where
 bitraverse f g (DNSProduction l rs) = DNSProduction <$> traverse f l <*> traverse (bitraverse f g) rs
 bitraverse f g (DNSVocabulary l ts) = DNSVocabulary <$> traverse f l <*> traverse (traverse g) ts

upcastDNSProduction :: DNSProduction True n t -> DNSProduction e n t
upcastDNSProduction (DNSProduction l rs) = DNSProduction l rs

downcastDNSProduction :: DNSProduction e n t -> Maybe (DNSProduction True n t)
downcastDNSProduction (DNSProduction l rs) = Just (DNSProduction l rs)
downcastDNSProduction (DNSVocabulary {})   = Nothing


-- ================================================================ --

-- |
data SomeDNSLHS n = forall l. SomeDNSLHS (DNSLHS l n)

instance (Show n) => Show (SomeDNSLHS n) where
 showsPrec d (SomeDNSLHS l) = showParen (d > 10)
  (showString "SomeDNSLHS " . showsPrec (10+1) l)

instance (Eq   n) => Eq   (SomeDNSLHS n) where SomeDNSLHS l1 == SomeDNSLHS l2 = l1 `equalDNSLHS` l2

instance Functor     SomeDNSLHS where fmap     = fmapDefault
instance Foldable    SomeDNSLHS where foldMap  = foldMapDefault
instance Traversable SomeDNSLHS where traverse f (SomeDNSLHS l) = SomeDNSLHS <$> traverse f l

-- ================================================================ --

-- | the "leaves" of the grammar.
data DNSToken t
 = DNSToken t -- ^ e.g. @"word or phrase"@
 | DNSPronounced t t -- ^ e.g. @written\\spoken@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

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
 deriving (Show,Eq)

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

-- | @emptyDNSRHS = DNSNonTerminal (SomeDNSLHS (DNSBuiltinList 'DNSEmptyList'))@
--
-- can be used as:
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
-- 'emptyDNSRHS', rather than the simple but non-@n@-polymorphic:
--
-- @
-- emptyDNSRHS :: DNSRHS String t
-- emptyDNSRHS = DNSNonTerminal (SomeDNSLHS (DNSList "emptyList"))
-- @
--
--
emptyDNSRHS :: DNSRHS n t
emptyDNSRHS = DNSNonTerminal (SomeDNSLHS (DNSBuiltinList DNSEmptyList))


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

deriving instance (Show n) => Show (DNSLHS l n)
instance (Eq n) => Eq (DNSLHS l n) where (==) = equalDNSLHS

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
