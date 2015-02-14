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
import Data.Traversable
import Prelude             hiding (mapM)


-- | a nonempty list of productions, with at least one export.
-- the evolution of the type (I like to know the provenance of
-- designs, it motivates the inevitable complexity, so I thought
-- I would try to do that too):
-- type DNSGrammar = [DNSProduction]
-- type DNSGrammar = (DNSLHS, [DNSProduction])
-- data DNSGrammar = DNSGrammar DNSLHS [DNSProduction]
-- data DNSGrammar (s :: Safety) = DNSGrammar DNSLHS [DNSProduction s]
-- = DNSGrammar (DNSProduction name token True) [forall exported. DNSProduction name token exported]
--
-- extra constraint that only one production is exported. I hope this
-- constraint isn't too restrictive for future uses.
--
data DNSGrammar name token = DNSGrammar
 { dnsExport      :: DNSProduction True name token
 , dnsProductions :: [DNSProduction False name token] -- TODO NonEmpty
 }

instance Bifunctor     DNSGrammar where  bimap     = bimapDefault
instance Bifoldable    DNSGrammar where  bifoldMap = bifoldMapDefault
instance Bitraversable DNSGrammar where -- valid Bitraversable?
 bitraverse f g (DNSGrammar production productions) =
  DNSGrammar <$> bitraverse f g production
             <*> traverse (bitraverse f g) productions

-- | you can import and export only 'DNSRule's, not 'DNSList's.
-- and obviously, 'DNSImport's can't be exported.
--
-- (does {exported} need distinct constructors, or a proxy like a Singleton)
-- I guess, to construct a DNSProduction with some desired {exported},
-- you just annotate the expression. And you can change a hidden production to of visible production with the specialized identity: when composing grammars.
--
-- e.g. @<rule> = ...;@
-- e.g. @<rule> exported = ...;@
--
-- e.g. @self.setList('list', [...])@
--
-- e.g. @<rule> imported;@
--
data DNSProduction exported name token where
 DNSProduction :: DNSLHS LHSRule name -> [DNSRHS name token] -> DNSProduction exported name token
 DNSVocabulary :: DNSLHS LHSList name -> [DNSToken token]    -> DNSProduction False name token
 DNSImport     :: DNSLHS LHSRule name     -> DNSProduction False name x
 -- TODO NonEmpty
 -- TODO NonEmpty

instance Bifunctor     (DNSProduction exported) where bimap     = bimapDefault
instance Bifoldable    (DNSProduction exported) where bifoldMap = bifoldMapDefault
instance Bitraversable (DNSProduction exported) where -- valid Bitraversable?
 bitraverse f g (DNSProduction l rs) = DNSProduction <$> traverse f l <*> traverse (bitraverse f g) rs
 bitraverse f g (DNSVocabulary l ts) = DNSVocabulary <$> traverse f l <*> traverse (traverse g) ts
 bitraverse f _ (DNSImport l)        = DNSImport     <$> traverse f l

-- | EBNF-like
data DNSRHS name token
 = DNSTerminal (DNSToken token) -- ^ e.g. @"terminal"@
 | forall lhs. DNSNonTerminal (DNSLHS lhs name) -- ^ e.g. @\<non_terminal>@ or @{non_terminal}@
 | DNSSequence [DNSRHS name token] -- ^ e.g. @first second ...@ -- TODO NonEmpty
 | DNSAlternatives [DNSRHS name token] -- ^ e.g. @(alternative | ...)@ -- TODO NonEmpty
 | DNSOptional (DNSRHS name token) -- ^ e.g. @[optional]@
 | DNSMultiple (DNSRHS name token) -- ^ e.g. @(multiple)+@


-- does ExistentialQuantification in DNSLHS allow exhaustive pattern matching?
 -- LHS is one of two, the GADT has only three constructors

instance Bifunctor     DNSRHS where bimap     = bimapDefault
instance Bifoldable    DNSRHS where bifoldMap = bifoldMapDefault
instance Bitraversable DNSRHS where -- valid Bitraversable?
 bitraverse _ g (DNSTerminal t)      = DNSTerminal     <$> traverse g t
 bitraverse f _ (DNSNonTerminal n)   = DNSNonTerminal  <$> traverse f n
 bitraverse f g (DNSSequence rs)     = DNSSequence     <$> traverse (bitraverse f g) rs
 bitraverse f g (DNSAlternatives rs) = DNSAlternatives <$> traverse (bitraverse f g) rs
 bitraverse f g (DNSOptional r)      = DNSOptional     <$> bitraverse f g r
 bitraverse f g (DNSMultiple r)      = DNSMultiple     <$> bitraverse f g r

data DNSToken token
 = DNSToken token -- ^ e.g. @"word or phrase"@
 | DNSPronounced token token -- ^ e.g. @written\\spoken@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | "isomorphic" to
-- @type DNSLHS s = Either (DNSLHSList s) (DNSLHSRule s)@.
-- But, to derive @Functor, Traversable@:
-- "Constructor ‘DNSLHS’ must use the type variable only as the
-- last argument of a data type"
--
--
-- e.g. @<rule>@
-- e.g. @<dgndictation>@
-- e.g. @{list}@
--
--
data DNSLHS lhs name where
 DNSRule    :: name       -> DNSLHS LHSRule name
 DNSBuiltin :: DNSBuiltin -> DNSLHS LHSRule x
 DNSList    :: name       -> DNSLHS LHSList name

-- | Can't make a derived instance of _: Constructor ‘_’ must not have existential arguments
-- https://ghc.haskell.org/trac/ghc/ticket/8678
-- deriving instance Functor     (DNSLHS lhs)
-- deriving instance Foldable    (DNSLHS lhs)
-- deriving instance Traversable (DNSLHS lhs)
instance Functor     (DNSLHS lhs) where fmap     = fmapDefault
instance Foldable    (DNSLHS lhs) where foldMap  = foldMapDefault
instance Traversable (DNSLHS lhs) where -- valid Traversable?
 traverse f (DNSRule name) = DNSRule <$> f name
 traverse f (DNSList name) = DNSList <$> f name
 traverse _ (DNSBuiltin x) = pure $ DNSBuiltin x

-- oh no, deriving two instances of child GADT won't work, the parent ADT can use those methods when existentially quantified over the phantom type. must be derived polymorphic way over anything

-- | Builtin 'DNSProduction's with 'DNSLHS's, but without
-- 'DNSRHS's.
data DNSBuiltin = DGNDictation | DGNWords | DGNLetters
 deriving (Show, Eq, Ord, Enum)

-- | don't use DataKinds in this module times because types share
-- names with constructors. Or can you?

-- | for promotion by @DataKinds@.
data LHSKind = LHSRule | LHSList
