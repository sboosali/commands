{-# LANGUAGE DataKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable  #-}
{-# LANGUAGE EmptyDataDecls, ExistentialQuantification, GADTs, RankNTypes #-}
{-# LANGUAGE StandaloneDeriving                                           #-}
module Commands.Frontends.Dragon13.Types where
import Commands.Etc        ()
import Commands.Instances  ()
import Control.Applicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable
import Data.List.NonEmpty  (NonEmpty (..))
import Data.Traversable
import Prelude             hiding (mapM)


-- | a Dragon NaturallySpeaking (\"DNS") grammar has
-- one or more 'DNSProduction's, exactly one of which is an export.
-- (the "exactly one" constraint is really "at least one", but
-- this restriction shouldn't matter).
--
-- in the type @DNSGrammar n t@, you can read:
--
-- * @n@ as @name@ or @nonTerminal@
-- * @t@ as @text@ or @terminal@
--
-- a 'Bitraversable', because the terminals and the non-terminals
-- are distinct lexemes, with different criteria for being valid.
--
data DNSGrammar n t = DNSGrammar
 { dnsExport      :: DNSProduction True n t
 , dnsProductions :: [DNSProduction False n t]
 }

instance Bifunctor     DNSGrammar where  bimap     = bimapDefault
instance Bifoldable    DNSGrammar where  bifoldMap = bifoldMapDefault
instance Bitraversable DNSGrammar where -- valid Bitraversable?
 bitraverse f g (DNSGrammar production productions) =
  DNSGrammar <$> bitraverse f g production
             <*> traverse (bitraverse f g) productions

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
-- * @"\<rule> = ...;"@ has type @'DNSProduction' 'False'@
-- * @"\<rule> exported = ...;"@ has type @'DNSProduction' 'True'@
-- * @"self.setList('list', [...])"@ is a 'DNSVocabulary'
-- * @"\<rule> imported;"@ is a 'DNSImport'
--
-- a @GADT@ to constrain the exportability and the right-hand sides of
-- its constructors.
-- you can import and export only 'DNSRule's (including 'DNSBuiltin's),
-- not 'DNSList's. a 'DNSVocabulary' can be named only by 'LHSList's, as
-- a 'DNSProduction' can be named only by 'LHSRule's.
--
data DNSProduction e n t where
 DNSProduction :: DNSLHS LHSRule n -> NonEmpty (DNSRHS n t) -> DNSProduction e     n t
 DNSVocabulary :: DNSLHS LHSList n -> NonEmpty (DNSToken t) -> DNSProduction False n t
 DNSImport     :: DNSLHS LHSRule n                          -> DNSProduction False n x

instance Bifunctor     (DNSProduction e) where bimap     = bimapDefault
instance Bifoldable    (DNSProduction e) where bifoldMap = bifoldMapDefault
instance Bitraversable (DNSProduction e) where
 bitraverse f g (DNSProduction l rs) = DNSProduction <$> traverse f l <*> traverse (bitraverse f g) rs
 bitraverse f g (DNSVocabulary l ts) = DNSVocabulary <$> traverse f l <*> traverse (traverse g) ts
 bitraverse f _ (DNSImport l)        = DNSImport     <$> traverse f l

-- | the
-- <https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form EBNF>-like
-- grammar specification.
--
data DNSRHS n t
 = DNSTerminal (DNSToken t) -- ^ e.g. @"terminal"@
 | forall l. DNSNonTerminal (DNSLHS l n) -- ^ e.g. @\<non_terminal>@ or @{non_terminal}@
 | DNSSequence (NonEmpty (DNSRHS n t)) -- ^ e.g. @first second ...@
 | DNSAlternatives (NonEmpty (DNSRHS n t)) -- ^ e.g. @(alternative | ...)@
 | DNSOptional (DNSRHS n t) -- ^ e.g. @[optional]@
 | DNSMultiple (DNSRHS n t) -- ^ e.g. @(multiple)+@

instance Bifunctor     DNSRHS where bimap     = bimapDefault
instance Bifoldable    DNSRHS where bifoldMap = bifoldMapDefault
instance Bitraversable DNSRHS where -- valid Bitraversable?
 bitraverse _ g (DNSTerminal t)      = DNSTerminal     <$> traverse g t
 bitraverse f _ (DNSNonTerminal n)   = DNSNonTerminal  <$> traverse f n
 bitraverse f g (DNSSequence rs)     = DNSSequence     <$> traverse (bitraverse f g) rs
 bitraverse f g (DNSAlternatives rs) = DNSAlternatives <$> traverse (bitraverse f g) rs
 bitraverse f g (DNSOptional r)      = DNSOptional     <$> bitraverse f g r
 bitraverse f g (DNSMultiple r)      = DNSMultiple     <$> bitraverse f g r

-- | the "leaves" of the grammar.
data DNSToken t
 = DNSToken t -- ^ e.g. @"word or phrase"@
 | DNSPronounced t t -- ^ e.g. @written\\spoken@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

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
-- * @"\<dgndictation>"@ is a 'DNSBuiltin'
-- * @"{list}@ is a 'DNSList
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
-- (see https://ghc.haskell.org/trac/ghc/ticket/8678)
data DNSLHS l n where
 DNSRule    :: n          -> DNSLHS LHSRule n
 DNSBuiltin :: DNSBuiltin -> DNSLHS LHSRule x
 DNSList    :: n          -> DNSLHS LHSList n

instance Functor     (DNSLHS lhs) where fmap     = fmapDefault
instance Foldable    (DNSLHS lhs) where foldMap  = foldMapDefault
instance Traversable (DNSLHS lhs) where
 traverse f (DNSRule name) = DNSRule <$> f name
 traverse f (DNSList name) = DNSList <$> f name
 traverse _ (DNSBuiltin x) = pure $ DNSBuiltin x

-- | Builtin 'DNSProduction's with 'DNSLHS's, but without
-- 'DNSRHS's.
data DNSBuiltin = DGNDictation | DGNWords | DGNLetters
 deriving (Show, Eq, Ord, Enum)

-- | for promotion by @DataKinds@.
data LHSKind = LHSRule | LHSList
