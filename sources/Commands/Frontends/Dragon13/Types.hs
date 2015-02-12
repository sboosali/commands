{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, RankNTypes #-}
module Commands.Frontends.Dragon13.Types where
import Commands.Etc
import Commands.Frontends.Dragon13.Text
import Commands.Instances               ()
import Data.Foldable
import Data.Traversable
import Prelude                          hiding (mapM)


-- | a nonempty list of productions, with at least one export.
-- the evolution of the type (I like to know the provenance of
-- designs, it motivates the inevitable complexity, so I thought
-- I would try to do that too):
-- type DragonGrammar = [DragonProduction]
-- type DragonGrammar = (DragonLHS, [DragonProduction])
-- data DragonGrammar = DragonGrammar DragonLHS [DragonProduction]
-- data DragonGrammar (s :: Safety) = DragonGrammar DragonLHS [DragonProduction s]
data DragonGrammar s = DragonGrammar (DragonExport s) [DragonProduction s]
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | isomorphic to (the constructor) 'DragonProduction'
data DragonExport s = DragonExport (DragonLHSRule s) (DragonRHS s) -- ^ e.g. @<rule> exported = ...;@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | you can both import and export 'DragonList's, just like 'DragonRule's.
data DragonProduction s
 = DragonProduction (DragonLHSRule s) (DragonRHS s) -- ^ e.g. @<rule> = ...;@
 | DragonVocabulary (DragonLHSList s) [DragonToken s]   -- ^ e.g. @{list} = ...;@
 | DragonImport (DragonLHS s) -- ^ e.g. @<rule> imported;@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | EBNF-like
data DragonRHS s
 = DragonTerminal (DragonToken s) -- ^ e.g. @"terminal"@
 | DragonNonTerminal (DragonLHS s) -- ^ e.g. @<non_terminal>@
 | DragonOptional (DragonRHS s) -- ^ e.g. @[optional]@
 | DragonMultiple (DragonRHS s) -- ^ e.g. @(multiple)+@
 | DragonAlternatives [DragonRHS s] -- ^ e.g. @(alternative | ...)@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data DragonToken s
 = DragonToken s -- ^ e.g. @"word or phrase"@
 | DragonPronounced s s -- ^ e.g. @written\spoken@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | "isomorphic" to
-- @type DragonLHS s = Either (DragonLHSList s) (DragonLHSRule s)@.
-- But, to derive @Functor, Traversable@:
-- "Constructor ‘DragonLHS’ must use the type variable only as the
-- last argument of a data type"
--
--
--
--
data DragonLHS s
 = DragonLHSList (DragonLHSList s)
 | DragonLHSRule (DragonLHSRule s)
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- |
data DragonLHSRule s
 = DragonRule s -- ^ e.g. @<rule>@
 | DragonBuiltin DragonBuiltin -- ^ e.g. @<dgndictation>@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- |
data DragonLHSList s = DragonList s -- ^ e.g. @{list}@
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Builtin 'DragonProduction's with 'DragonLHS's, but without
-- 'DragonRHS's.
data DragonBuiltin = DGNDictation | DGNWords | DGNLetters
 deriving (Show, Eq, Ord, Enum)

-- |
-- AMP..
escapeDragonGrammar :: DragonGrammar Text -> Possibly (DragonGrammar DragonText)
escapeDragonGrammar = mapM escapeDragonText
