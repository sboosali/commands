{-# LANGUAGE DataKinds, DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs         #-}
{-# LANGUAGE KindSignatures, LambdaCase, RankNTypes, StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell                                             #-}
module Commands.Frontends.Dragon13.Types where
import Commands.Etc        ()
import Commands.Instances  ()

import Control.Applicative
import Control.Lens
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
-- in the type @DNSLHS l n@, you can read:
--
-- * @l@ as @leftHandSide@
-- * @n@ as @name@ or @nonTerminal@
--
-- in relation to NatLink's concrete syntax:
--
-- * @"\<rule>"@ is a 'DNSRule'
-- * @"\<dgndictation>"@ is a 'DNSBuiltin'
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
 DNSBuiltin :: DNSBuiltin -> DNSLHS LHSRule x
 DNSRule    :: n          -> DNSLHS LHSRule n
 DNSList    :: n          -> DNSLHS LHSList n

deriving instance (Show n) => Show (DNSLHS l n)
instance (Eq n) => Eq (DNSLHS l n) where (==) = equalDNSLHS

instance Functor     (DNSLHS l) where fmap     = fmapDefault
instance Foldable    (DNSLHS l) where foldMap  = foldMapDefault
instance Traversable (DNSLHS l) where
 traverse f (DNSRule n) = DNSRule <$> f n
 traverse f (DNSList n) = DNSList <$> f n
 traverse _ (DNSBuiltin x) = pure $ DNSBuiltin x

-- | heterogeneous (but only in the phantom) equality
equalDNSLHS :: (Eq n) => DNSLHS l1 n -> DNSLHS l2 n -> Bool
DNSRule    x `equalDNSLHS` DNSRule    y = x == y
DNSBuiltin x `equalDNSLHS` DNSBuiltin y = x == y
DNSList    x `equalDNSLHS` DNSList    y = x == y
_            `equalDNSLHS` _            = False

-- not a Prism because it merges two cases
dnsLHSName :: Getter (DNSLHS l n) (Maybe n)
dnsLHSName = to $ \case
 DNSBuiltin _ -> Nothing
 DNSRule n    -> Just n
 DNSList n    -> Just n

-- | Builtin 'DNSProduction's: they have left-hand sides,
-- but they don't have right-hand sides.
data DNSBuiltin = DGNDictation | DGNWords | DGNLetters
 deriving (Show, Eq, Ord, Enum)

displayDNSBuiltin :: DNSBuiltin -> String
displayDNSBuiltin = fmap toLower . show

-- | for promotion by @DataKinds@.
--
-- 'LHSRule's and 'LHSList's seem to share different namespaces
-- in Dragon NaturallySpeaking.
--
data LHSKind = LHSRule | LHSList

-- ================================================================ --

-- |
data SomeDNSLHS n = forall l. SomeDNSLHS (DNSLHS l n)

instance (Show n) => Show (SomeDNSLHS n) where show (SomeDNSLHS l) = show l
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

makePrisms ''DNSRHS

deriving instance (Show n, Show t) => Show (DNSRHS n t)
deriving instance (Eq   n, Eq   t) => Eq   (DNSRHS n t)

instance Bifunctor     DNSRHS where bimap     = bimapDefault
instance Bifoldable    DNSRHS where bifoldMap = bifoldMapDefault
instance Bitraversable DNSRHS where -- valid Bitraversable?
 bitraverse _ g (DNSTerminal t)      = DNSTerminal     <$> traverse g t
 bitraverse f _ (DNSNonTerminal n)   = DNSNonTerminal  <$> traverse f n
 bitraverse f g (DNSSequence rs)     = DNSSequence     <$> traverse (bitraverse f g) rs
 bitraverse f g (DNSAlternatives rs) = DNSAlternatives <$> traverse (bitraverse f g) rs
 bitraverse f g (DNSOptional r)      = DNSOptional     <$> bitraverse f g r
 bitraverse f g (DNSMultiple r)      = DNSMultiple     <$> bitraverse f g r

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
-- an 'LHSRule' \'s 'DNSRHS' must be 'NonEmpty', but an 'LHSList' \'s 'DNSToken's may be empty.
--
data DNSProduction (e :: Bool) n t where
 DNSProduction :: DNSLHS LHSRule n -> NonEmpty (DNSRHS n t) -> DNSProduction e     n t
 DNSVocabulary :: DNSLHS LHSList n -> [DNSToken t]          -> DNSProduction False n t
 DNSImport     :: DNSLHS LHSRule n                          -> DNSProduction False n x

-- makePrisms ''DNSProduction

-- equalDNSProduction :: (Eq n) => DNSProduction e1 n t1 -> DNSProduction e2 n t2 -> Bool
-- equalDNSProduction x y = lhsOfDNSProduction x == lhsOfDNSProduction y

-- lhsOfDNSProduction :: DNSProduction e n t -> SomeDNSLHS n
-- lhsOfDNSProduction (DNSProduction l _) = SomeDNSLHS l
-- lhsOfDNSProduction (DNSVocabulary l _) = SomeDNSLHS l
-- lhsOfDNSProduction (DNSImport l)       = SomeDNSLHS l

deriving instance (Show n, Show t) => Show (DNSProduction e n t)
deriving instance (Eq   n, Eq   t) => Eq   (DNSProduction e n t)

instance Bifunctor     (DNSProduction e) where bimap     = bimapDefault
instance Bifoldable    (DNSProduction e) where bifoldMap = bifoldMapDefault
instance Bitraversable (DNSProduction e) where
 bitraverse f g (DNSProduction l rs) = DNSProduction <$> traverse f l <*> traverse (bitraverse f g) rs
 bitraverse f g (DNSVocabulary l ts) = DNSVocabulary <$> traverse f l <*> traverse (traverse g) ts
 bitraverse f _ (DNSImport l)        = DNSImport     <$> traverse f l

-- nameOfDNSProduction :: DNSProduction e n t -> SomeDNSLHS n
-- nameOfDNSProduction (DNSProduction l _) = SomeDNSLHS l
-- nameOfDNSProduction (DNSVocabulary l _) = SomeDNSLHS l
-- nameOfDNSProduction (DNSImport l)       = SomeDNSLHS l

upcastDNSProduction :: DNSProduction True n t -> DNSProduction e n t
upcastDNSProduction (DNSProduction l rs) = DNSProduction l rs

dnsProductionName :: Getter (DNSProduction e n t) (Maybe n)
dnsProductionName = to $ \case
 DNSProduction l _ -> l ^. dnsLHSName
 DNSVocabulary l _ -> l ^. dnsLHSName
 (DNSImport l) -> l ^. dnsLHSName

dnsProductionNonTerminals :: Traversal' (DNSProduction e n t) (SomeDNSLHS n)
dnsProductionNonTerminals = undefined
 -- _DNSProductionRHS.traversed._DNSNonTerminal.
 -- make sure the Traversal matches DNSNonTerminal's within other DNSRHS's

-- not Getter, because we want to set
-- not Prism', RHS (without LHS) is not sufficient to inject
-- yes a Traversal', it can have multiple targets and set them. what are its laws?
-- _DNSProductionRHS :: Prism' (DNSProduction e n t) (DNSRHS n)
-- _DNSProductionRHS =

-- ================================================================ --

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
 , _dnsProductions :: [DNSProduction False n t]
 }
 deriving (Show, Eq)

makeLenses ''DNSGrammar

instance Bifunctor     DNSGrammar where  bimap     = bimapDefault
instance Bifoldable    DNSGrammar where  bifoldMap = bifoldMapDefault
instance Bitraversable DNSGrammar where -- valid Bitraversable?
 bitraverse f g (DNSGrammar production productions) =
  DNSGrammar <$> bitraverse f g production
             <*> traverse (bitraverse f g) productions
